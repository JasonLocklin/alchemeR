#!/usr/bin/env Rscript
# Copy-paste starting point for a scheduled ingest job (cron, Task Scheduler,
# GitHub Actions, etc). Configuration comes entirely from the environment --
# see inst/extdata/Renviron.example -- so this script has nothing
# account-specific to edit.
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
  "Application database: {status$n_surveys} surveys, {status$n_responses} responses, ",
  "{status$size_bytes} bytes on disk."
))

checks <- db_check()
if (!all(checks$passed)) {
  cli::cli_warn("db_check() found integrity failures:")
  print(checks[!checks$passed, ])
}
