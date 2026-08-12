#!/usr/bin/env Rscript
# Copy-paste starting point for a full Extract-Transform-Load pipeline:
# Alchemer -> local application database -> an analytics database (shown
# here as Microsoft SQL Server via odbc, but load_pub_layer()/
# load_pipeline_health() are plain DBI and work against any DBI backend).
#
# All configuration comes from the environment -- see
# inst/extdata/Renviron.example -- so this script has nothing
# account/server-specific to edit. The destination service account needs
# only CREATE TABLE / DROP TABLE / INSERT privileges: every load is a full
# overwrite (DBI::dbWriteTable(overwrite = TRUE)), and nothing here ever
# reads data back from the destination to decide what to do -- all pipeline
# state lives in the local application database (ALCHEMER_DB).
#
# Example crontab entry (every 15 minutes):
#   */15 * * * * Rscript /path/to/etl_pipeline.R >> /path/to/etl.log 2>&1

library(alchemeR)

# --- Extract + Transform ------------------------------------------------

result <- ingest()
cli::cli_inform(paste0(
  "ingest(): {sum(result$status == 'ok')} refreshed, ",
  "{sum(result$status == 'skipped')} skipped, {sum(result$status == 'error')} failed."
))

pub_layer()

checks <- db_check()
if (!all(checks$passed)) {
  cli::cli_warn(paste0(
    "db_check() found integrity failures -- loading proceeds anyway; ",
    "investigate raw/meta before trusting the results."
  ))
  print(checks[!checks$passed, ])
}

# --- Load ----------------------------------------------------------------
# odbc + a DSN (or a full connection string) is one way to reach SQL Server;
# any DBI backend works identically for the calls below.

dest <- DBI::dbConnect(
  odbc::odbc(),
  Driver = Sys.getenv("ANALYTICS_DB_DRIVER", "ODBC Driver 18 for SQL Server"),
  Server = Sys.getenv("ANALYTICS_DB_SERVER"),
  Database = Sys.getenv("ANALYTICS_DB_DATABASE"),
  UID = Sys.getenv("ANALYTICS_DB_UID"),
  PWD = Sys.getenv("ANALYTICS_DB_PWD"), # or keyring::key_get("analytics_db", "password")
  TrustServerCertificate = "yes"
)
on.exit(DBI::dbDisconnect(dest), add = TRUE)

schema <- Sys.getenv("ANALYTICS_DB_SCHEMA", "dbo")
load_result <- load_pub_layer(dest, schema = schema)
cli::cli_inform("Loaded {nrow(load_result)} table(s): {sum(load_result$n_rows)} total rows.")

load_pipeline_health(dest, schema = schema)
