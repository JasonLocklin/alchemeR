# Idempotent DDL for the raw/meta/pub schemas. DuckLake supports no primary
# key, unique, foreign key, or check constraints (ADR-006) -- uniqueness and
# referential integrity are asserted after the fact by db_check(), not
# declared here. Every `raw` column is VARCHAR or JSON (ADR-003); typing
# happens only in `pub`, built separately by pub_layer().

schema_version <- 2L

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
    ingested_at TIMESTAMP, run_id VARCHAR"
  # No `contacts`/`contact_lists` here. They were declared before anything
  # fetched them, so every database shipped two permanently empty tables that
  # looked like a feature. Contacts carry heavy PII (ADR-009) and would need an
  # explicit opt-in; declare the tables in the same change that populates them.
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
  # One row per load_pub_layer() run. This is the only record of the Load
  # stage anywhere the pipeline can read it: the destination service account
  # is write-only by design, so `tables` (a JSON array of the destination
  # table names written) is also what lets the next load drop tables it
  # previously wrote and no longer produces -- e.g. a wide view whose survey
  # was retitled -- without ever guessing at names it doesn't own.
  loads = "
    load_id VARCHAR, started_at TIMESTAMP, finished_at TIMESTAMP,
    status VARCHAR, destination VARCHAR, n_tables INTEGER, n_rows INTEGER,
    tables JSON, message VARCHAR",
  # One row per survey built into `pub`, recording what it was built *from* and
  # *with* (ADR-017). pub_layer() rebuilds a survey only when one of these no
  # longer matches, so a run where nothing changed upstream does no work.
  # Every column is part of that comparison: `source_watermark` covers the raw
  # rows, the other four cover the build itself -- a different `language`,
  # `tz`, or `wide_views` setting produces different output from identical
  # input, and a package upgrade may change the pub SQL entirely.
  pub_state = "
    survey_id VARCHAR, source_watermark VARCHAR, language VARCHAR, tz VARCHAR,
    wide_views BOOLEAN, package_version VARCHAR, built_at TIMESTAMP",
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

# The natural key of each raw table, used to match a fetched row against the
# stored one (ADR-005). DuckLake declares no primary keys, so these are
# asserted after the fact by db_check() rather than enforced here.
raw_table_keys <- list(
  surveys = "survey_id",
  survey_definitions = "survey_id",
  survey_pages = c("survey_id", "page_id"),
  survey_questions = c("survey_id", "question_id"),
  survey_question_options = c("survey_id", "question_id", "option_id"),
  responses = c("survey_id", "response_id"),
  survey_statistics = c("survey_id", "question_id"),
  survey_campaigns = c("survey_id", "campaign_id")
)

# Columns describing *when we stored* a row rather than what Alchemer said, so
# they are excluded when deciding whether a fetched row differs from the stored
# one. Including them would make every row differ on every run -- `ingested_at`
# is stamped fresh each time -- which is exactly the write amplification the
# merge exists to avoid.
bookkeeping_columns <- c("ingested_at", "run_id")
# pub_layer.R's writes are hand-written INSERT ... BY NAME SELECT statements,
# not a registered-data.frame INSERT ... SELECT * -- BY NAME already matches
# by column name (and fails loudly on a mismatch) with no positional-order
# risk to guard against, so there is no pub_table_columns here to parallel
# raw_table_columns/meta_table_columns above.

# For read-only callers, which can't run ensure_schema(): a database last
# opened by an older version of the package is missing any table added since,
# and a read-only connection has no way to add it. Reading such a table has to
# degrade to "no rows", not error.
table_exists <- function(con, schema, table) {
  DBI::dbGetQuery(con, glue::glue(
    "SELECT COUNT(*) AS n FROM information_schema.tables
     WHERE table_catalog = {DBI::dbQuoteString(con, ducklake_alias)}
       AND table_schema = {DBI::dbQuoteString(con, schema)}
       AND table_name = {DBI::dbQuoteString(con, table)}"
  ))$n > 0
}

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
  } else if (current[1] < schema_version) {
    # Version 1 -> 2 adds meta.pub_state, which create_tables() has just
    # created empty. No data migration is needed or wanted: an empty
    # pub_state means "no survey has a recorded build", so the next
    # pub_layer() rebuilds everything once and records it. That is the
    # correct answer -- nothing here knows what the existing `pub` rows were
    # built from, and assuming they are current is exactly the stale-data
    # risk this table exists to remove.
    DBI::dbExecute(con, glue::glue(
      "UPDATE {ducklake_alias}.meta.schema_version SET version = {schema_version}"
    ))
  }
  invisible(TRUE)
}

# A short unique string for scoping a temp table/view name to one call, so
# concurrent writes on the same connection (there are none today, but the
# pattern is shared by db_schema.R and ingest.R) can't collide.
#
# Uses tempfile() rather than sample(letters): sample() draws from the user's
# RNG stream, so every ingest() silently changed the random numbers a script
# would get afterwards -- a surprising side effect from a function that only
# needs a name nobody else is using.
random_suffix <- function(n = 12) {
  substr(gsub("^file", "", basename(tempfile(pattern = "file"))), 1, n)
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

# Reconcile a fetched set of rows against what is stored, writing only what
# actually changed.
#
# Every refresh still downloads *everything* for the survey (ADR-004 is
# untouched -- this is about what gets written, never about what gets asked
# for). What changed is that the stored rows are no longer deleted and
# rewritten wholesale. DuckLake never modifies a Parquet file in place, so a
# delete-then-insert of a whole survey writes a fresh copy of every row on
# every run: a 5,000-response survey polled every 15 minutes cost ~2 MB per
# refresh, ~200 MB/day, all of it retained until snapshots expire. Measured
# over 12 refreshes of such a survey: 2.1 MB -> 26.7 MB replacing, versus
# 2.1 MB -> 2.1 MB merging.
#
# `on_vanished` decides what happens to rows that are stored but absent from
# the fetch -- the set difference that is the only way to detect an upstream
# deletion (ADR-005):
#   "flag"   is_deleted = TRUE, never removed (responses, surveys)
#   "delete" removed, matching the previous replace-wholesale behaviour for
#            definition/statistics/campaign rows, which are derived metadata
#            rather than research data
#
# A row that reappears upstream is un-flagged automatically, because
# `is_deleted` is one of the compared columns and the fetched row always
# carries FALSE.
merge_survey_rows <- function(con, table, survey_id, rows, on_vanished = c("delete", "flag"),
                              now = Sys.time()) {
  on_vanished <- match.arg(on_vanished)
  columns <- raw_table_columns[[table]]
  keys <- raw_table_keys[[table]]
  scope <- if (is.null(survey_id)) {
    ""
  } else {
    glue::glue("survey_id = {DBI::dbQuoteString(con, survey_id)} AND")
  }

  # Nothing fetched -- which is a real state, not an error: a survey whose
  # responses were all deleted, or an account whose survey list came back
  # empty. Every stored row in scope has vanished, and saying so directly
  # avoids registering an empty frame just to take a set difference against
  # it. `ncol(rows) == 0` covers a parse that produced no columns at all
  # (an empty bind_rows()), which is the same case arriving by another route.
  if (ncol(rows) == 0 || nrow(rows) == 0) {
    flag_or_delete_vanished(con, table, on_vanished, scope, absent = "TRUE", now = now)
    return(invisible(NULL))
  }

  rows <- as.data.frame(dplyr::select(rows, dplyr::all_of(columns)))
  batch <- paste0("tmp_merge_", table, "_", random_suffix())
  duckdb::duckdb_register(con, batch, rows)
  on.exit(duckdb::duckdb_unregister(con, batch), add = TRUE)

  key_match <- paste(glue::glue("t.{keys} = s.{keys}"), collapse = " AND ")
  compared <- setdiff(columns, c(keys, bookkeeping_columns))
  changed <- paste(glue::glue("t.{compared} IS DISTINCT FROM s.{compared}"), collapse = " OR ")

  DBI::dbExecute(con, glue::glue(
    "MERGE INTO {ducklake_alias}.raw.{table} t USING {batch} s ON ({key_match})
     WHEN MATCHED AND ({changed}) THEN UPDATE
     WHEN NOT MATCHED THEN INSERT"
  ))

  key_tuple <- paste(keys, collapse = ", ")
  absent <- glue::glue(
    "({key_tuple}) NOT IN (SELECT {key_tuple} FROM {batch})"
  )
  flag_or_delete_vanished(con, table, on_vanished, scope, absent, now)
  invisible(NULL)
}

flag_or_delete_vanished <- function(con, table, on_vanished, scope, absent, now) {
  if (identical(on_vanished, "flag")) {
    DBI::dbExecute(con, glue::glue(
      "UPDATE {ducklake_alias}.raw.{table}
       SET is_deleted = TRUE, deleted_detected_at = {DBI::dbQuoteLiteral(con, now)}
       WHERE {scope} (is_deleted IS NULL OR NOT is_deleted) AND {absent}"
    ))
  } else {
    DBI::dbExecute(con, glue::glue(
      "DELETE FROM {ducklake_alias}.raw.{table} WHERE {scope} {absent}"
    ))
  }
  invisible(NULL)
}
