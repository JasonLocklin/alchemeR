# Idempotent DDL for the raw/meta/pub schemas. DuckLake supports no primary
# key, unique, foreign key, or check constraints (ADR-006) -- uniqueness and
# referential integrity are asserted after the fact by db_check(), not
# declared here. Every `raw` column is VARCHAR or JSON (ADR-003); typing
# happens only in `pub`, built separately by pub_layer().

schema_version <- 1L

raw_tables <- list(
  surveys = "
    survey_id VARCHAR, title VARCHAR, type VARCHAR, status VARCHAR,
    created_on VARCHAR, modified_on VARCHAR, team VARCHAR,
    statistics JSON, links JSON, payload JSON,
    ingested_at TIMESTAMP, run_id VARCHAR,
    is_deleted BOOLEAN, deleted_detected_at TIMESTAMP",
  survey_definitions = "
    survey_id VARCHAR, payload JSON,
    ingested_at TIMESTAMP, run_id VARCHAR",
  survey_pages = "
    survey_id VARCHAR, page_id VARCHAR, title JSON, description VARCHAR,
    properties JSON, page_order INTEGER, payload JSON,
    ingested_at TIMESTAMP, run_id VARCHAR",
  survey_questions = "
    survey_id VARCHAR, question_id VARCHAR, page_id VARCHAR,
    base_type VARCHAR, type VARCHAR, title JSON, shortname VARCHAR,
    varname VARCHAR, description VARCHAR, properties JSON,
    show_rules_ids JSON, question_order INTEGER, payload JSON,
    ingested_at TIMESTAMP, run_id VARCHAR",
  survey_question_options = "
    survey_id VARCHAR, question_id VARCHAR, option_id VARCHAR, title JSON,
    value VARCHAR, properties JSON, option_order INTEGER, payload JSON,
    ingested_at TIMESTAMP, run_id VARCHAR",
  responses = "
    survey_id VARCHAR, response_id VARCHAR, status VARCHAR,
    is_test_data VARCHAR, date_submitted VARCHAR, date_started VARCHAR,
    date_updated VARCHAR, session_id VARCHAR, language VARCHAR,
    link_id VARCHAR, contact_id VARCHAR, response_time VARCHAR,
    url_variables JSON, data_quality JSON, survey_data JSON, payload JSON,
    ingested_at TIMESTAMP, run_id VARCHAR,
    is_deleted BOOLEAN, deleted_detected_at TIMESTAMP",
  survey_statistics = "
    survey_id VARCHAR, question_id VARCHAR, type VARCHAR, stats JSON,
    ingested_at TIMESTAMP, run_id VARCHAR",
  survey_campaigns = "
    survey_id VARCHAR, campaign_id VARCHAR, payload JSON,
    ingested_at TIMESTAMP, run_id VARCHAR",
  contacts = "
    contact_id VARCHAR, list_id VARCHAR, payload JSON,
    ingested_at TIMESTAMP, run_id VARCHAR",
  contact_lists = "
    list_id VARCHAR, list_name VARCHAR, payload JSON,
    ingested_at TIMESTAMP, run_id VARCHAR"
)

meta_tables <- list(
  runs = "
    run_id VARCHAR, started_at TIMESTAMP, finished_at TIMESTAMP,
    status VARCHAR, package_version VARCHAR, duckdb_version VARCHAR,
    n_checked INTEGER, n_refreshed INTEGER, n_failed INTEGER, n_requests INTEGER",
  run_events = "
    run_id VARCHAR, survey_id VARCHAR, phase VARCHAR, status VARCHAR,
    http_status INTEGER, message VARCHAR,
    started_at TIMESTAMP, finished_at TIMESTAMP, n_responses INTEGER",
  survey_state = "
    survey_id VARCHAR, last_modified_on VARCHAR,
    last_probe_total_count INTEGER, last_probe_max_date_updated VARCHAR,
    last_refresh_started_at TIMESTAMP, last_successful_refresh_at TIMESTAMP,
    consecutive_failures INTEGER",
  integrity_checks = "
    run_id VARCHAR, survey_id VARCHAR, check_name VARCHAR,
    passed BOOLEAN, message VARCHAR, checked_at TIMESTAMP",
  schema_version = "version INTEGER"
)

# Typed, language-resolved (ADR-008); built by pub_layer(), never by
# ingest(). Unlike raw/meta, this schema has no fixed table set the way
# raw/meta do beyond these five -- pub_layer() only ever writes here.
pub_tables <- list(
  surveys = "
    survey_id VARCHAR, title VARCHAR, type VARCHAR, status VARCHAR,
    created_on TIMESTAMP, modified_on TIMESTAMP, team VARCHAR, is_deleted BOOLEAN",
  questions = "
    survey_id VARCHAR, question_id VARCHAR, page_id VARCHAR, type VARCHAR,
    title VARCHAR, shortname VARCHAR, varname VARCHAR, question_order INTEGER",
  options = "
    survey_id VARCHAR, question_id VARCHAR, option_id VARCHAR, title VARCHAR,
    value VARCHAR, option_order INTEGER",
  responses = "
    survey_id VARCHAR, response_id VARCHAR, status VARCHAR, is_test_data BOOLEAN,
    date_submitted TIMESTAMP, date_started TIMESTAMP, date_updated TIMESTAMP,
    session_id VARCHAR, language VARCHAR, link_id VARCHAR, contact_id VARCHAR,
    response_time INTEGER, is_deleted BOOLEAN",
  answers = "
    survey_id VARCHAR, response_id VARCHAR, question_id VARCHAR,
    question_shortname VARCHAR, question_title VARCHAR, option_id VARCHAR,
    answer VARCHAR, reporting_value VARCHAR, shown BOOLEAN, is_deleted BOOLEAN"
)

# Exact column order for each raw.*/meta.* table, parsed once from the DDL
# above. ingest() uses this to reorder/validate a tibble before writing it,
# since the INSERT ... SELECT * FROM <registered data.frame> pattern maps
# columns by *position*, not name -- a tibble built in a different column
# order would silently write values into the wrong columns instead of
# failing loudly (or would fail loudly with a confusing type-mismatch error,
# which is how this helper earned its comment).
table_columns <- function(ddl) {
  unname(vapply(
    strsplit(ddl, ",")[[1]],
    function(col) strsplit(trimws(col), "\\s+")[[1]][1],
    character(1)
  ))
}
raw_table_columns <- lapply(raw_tables, table_columns)
meta_table_columns <- lapply(meta_tables, table_columns)
# pub_layer.R's writes are hand-written INSERT ... BY NAME SELECT statements,
# not a registered-data.frame INSERT ... SELECT * -- BY NAME already matches
# by column name (and fails loudly on a mismatch) with no positional-order
# risk to guard against, so there is no pub_table_columns here to parallel
# raw_table_columns/meta_table_columns above.

create_tables <- function(con, schema, tables) {
  DBI::dbExecute(con, glue::glue("CREATE SCHEMA IF NOT EXISTS {ducklake_alias}.{schema}"))
  for (name in names(tables)) {
    DBI::dbExecute(con, glue::glue(
      "CREATE TABLE IF NOT EXISTS {ducklake_alias}.{schema}.{name} ({tables[[name]]})"
    ))
  }
}

# Runs on every non-read-only alchemer_db() call. Safe to re-run: every
# statement is IF NOT EXISTS, so a normal connection never issues DDL after
# the first time it creates a fresh database.
ensure_schema <- function(con) {
  create_tables(con, "raw", raw_tables)
  create_tables(con, "meta", meta_tables)
  create_tables(con, "pub", pub_tables)

  current <- DBI::dbGetQuery(
    con,
    glue::glue("SELECT version FROM {ducklake_alias}.meta.schema_version")
  )$version
  if (length(current) == 0) {
    DBI::dbExecute(con, glue::glue(
      "INSERT INTO {ducklake_alias}.meta.schema_version VALUES ({schema_version})"
    ))
  }
  # Future schema migrations key off `current` here; there is only one
  # version so far.
  invisible(TRUE)
}

# A short random string for scoping a temp table/view name to one call, so
# concurrent writes on the same connection (there are none today, but the
# pattern is shared by db_schema.R and ingest.R) can't collide.
random_suffix <- function(n = 12) {
  paste(sample(letters, n, replace = TRUE), collapse = "")
}

# Appends `rows` to raw.<table>, reordered/validated against the table's
# declared column order (raw_table_columns) -- required because
# INSERT ... SELECT * FROM <registered data.frame> maps columns positionally,
# not by name, so a tibble built in the wrong order would silently write
# values into the wrong columns instead of failing.
write_rows <- function(con, table, rows) {
  if (nrow(rows) == 0) {
    return(invisible(NULL))
  }
  rows <- as.data.frame(dplyr::select(rows, dplyr::all_of(raw_table_columns[[table]])))
  tmp_name <- paste0("tmp_write_", table, "_", random_suffix())
  duckdb::duckdb_register(con, tmp_name, rows)
  DBI::dbExecute(con, glue::glue("INSERT INTO {ducklake_alias}.raw.{table} SELECT * FROM {tmp_name}"))
  duckdb::duckdb_unregister(con, tmp_name)
  invisible(NULL)
}

# Replace one survey's rows in a raw.* table: delete what's there, write the
# new set. Used for every raw table except responses and surveys, which have
# their own carry-forward-deleted logic (ADR-005) instead of a plain replace.
replace_survey_rows <- function(con, table, survey_id, rows) {
  DBI::dbExecute(con, glue::glue(
    "DELETE FROM {ducklake_alias}.raw.{table} WHERE survey_id = {DBI::dbQuoteString(con, survey_id)}"
  ))
  write_rows(con, table, rows)
}
