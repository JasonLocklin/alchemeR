#!/usr/bin/env Rscript
# A complete pipeline: Alchemer -> local archive -> analytics database.
#
# Stages, in ETL terms:
#   Extract + Load   ingest()          Alchemer API -> local `raw`, verbatim
#   Transform        pub_layer()       local `raw`  -> local `pub`, typed
#   Load             load_pub_layer()  local `pub`  -> analytics database
#
# All configuration comes from the environment (see Renviron.example), so there
# is nothing account- or server-specific to edit here. The destination service
# account needs only CREATE TABLE / DROP TABLE / INSERT: every load is a full
# overwrite, and nothing reads the destination back to decide what to do -- all
# pipeline state lives in the local application database (ALCHEMER_DB).
#
# Example crontab entry (every 15 minutes):
#   */15 * * * * Rscript /path/to/etl_pipeline.R >> /path/to/etl.log 2>&1

library(alchemeR)

# --- Extract + Load --------------------------------------------------------
# Nothing is transformed here: `raw` gets Alchemer's payloads as they arrived,
# so a new question type can never fail a run.

result <- ingest()
cli::cli_inform(paste0(
  "ingest(): {sum(result$status == 'ok')} refreshed, ",
  "{sum(result$status == 'skipped')} skipped, {sum(result$status == 'error')} failed."
))

# --- Transform -------------------------------------------------------------
# Only surveys whose `raw` rows actually moved are rebuilt, so this is cheap on
# a tick where little or nothing changed upstream.

rebuilt <- pub_layer()
cli::cli_inform("pub_layer(): {length(rebuilt)} survey(s) rebuilt, the rest unchanged.")

checks <- db_check()
if (!all(checks$passed)) {
  cli::cli_warn("db_check() found integrity failures; investigate raw/meta.")
  print(checks[!checks$passed, ])
}

# --- Load ------------------------------------------------------------------
# odbc + SQL Server shown here; load_pub_layer() takes any DBI connection.

dest <- DBI::dbConnect(
  odbc::odbc(),
  Driver = Sys.getenv("ANALYTICS_DB_DRIVER", "ODBC Driver 18 for SQL Server"),
  Server = Sys.getenv("ANALYTICS_DB_SERVER"),
  Database = Sys.getenv("ANALYTICS_DB_DATABASE"),
  UID = Sys.getenv("ANALYTICS_DB_UID"),
  PWD = Sys.getenv("ANALYTICS_DB_PWD"), # or keyring::key_get("analytics_db", "password")
  TrustServerCertificate = "yes"
)

# `finally`, not on.exit(): on.exit() only fires when a *function* exits, so it
# does nothing at the top level of a script.
tryCatch(
  {
    schema <- Sys.getenv("ANALYTICS_DB_SCHEMA", "dbo")
    load_result <- load_pub_layer(dest, schema = schema)
    cli::cli_inform("Loaded {nrow(load_result)} table(s): {sum(load_result$n_rows)} total rows.")

    # A row for monitoring: the outcome of both stages, readable from the
    # analytics database alone.
    load_pipeline_health(dest, schema = schema)
  },
  finally = DBI::dbDisconnect(dest)
)
