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
  DBI::dbExecute(con, glue::glue("CREATE SCHEMA IF NOT EXISTS {ducklake_alias}.pub"))

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
