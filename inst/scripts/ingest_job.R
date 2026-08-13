#!/usr/bin/env Rscript
# A scheduled job that keeps the local archive current: Alchemer -> `raw`
# (verbatim) -> `pub` (tidy and typed). No analytics database involved -- see
# etl_pipeline.R for that.
#
# All configuration comes from the environment (see Renviron.example), so there
# is nothing account-specific to edit here.
#
# Example crontab entry (every 15 minutes):
#   */15 * * * * Rscript /path/to/ingest_job.R >> /path/to/ingest.log 2>&1

library(alchemeR)

result <- ingest()
cli::cli_inform(paste0(
  "ingest() considered {nrow(result)} survey(s): {sum(result$status == 'ok')} refreshed, ",
  "{sum(result$status == 'skipped')} skipped, {sum(result$status == 'error')} failed."
))
if (any(result$status == "error")) {
  print(result[result$status == "error", c("survey_id", "decision", "status")])
}

pub_layer()

status <- db_status()
cli::cli_inform(paste0(
  "Archive: {status$n_surveys} surveys, {status$n_responses} responses, ",
  "{round(status$size_bytes / 1e6)} MB on disk."
))

checks <- db_check()
if (!all(checks$passed)) {
  cli::cli_warn("db_check() found integrity failures:")
  print(checks[!checks$passed, ])
}
