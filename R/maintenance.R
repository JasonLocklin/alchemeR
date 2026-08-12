# Time-travel depth and expungement share one knob (§8 of the spec):
# history only reaches as far back as retention allows, and expire_history()
# is what trims it. expunge() goes further -- it removes rows *and* their
# history, for research data that must not merely become invisible but
# actually leave the database.

#' Application database summary
#'
#' @param db Application database directory. Defaults to [alchemer_db_path()].
#' @return A one-row tibble: survey/response counts (live and flagged
#'   deleted), snapshot count, on-disk size, and the most recent run.
#' @export
db_status <- function(db = alchemer_db_path()) {
  con <- alchemer_db(db, read_only = TRUE)
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))

  surveys <- DBI::dbGetQuery(con, glue::glue(
    "SELECT COUNT(*) AS n, SUM(CASE WHEN is_deleted THEN 1 ELSE 0 END) AS n_deleted
     FROM {ducklake_alias}.raw.surveys"
  ))
  responses <- DBI::dbGetQuery(con, glue::glue(
    "SELECT COUNT(*) AS n, SUM(CASE WHEN is_deleted THEN 1 ELSE 0 END) AS n_deleted
     FROM {ducklake_alias}.raw.responses"
  ))
  last_run <- DBI::dbGetQuery(con, glue::glue(
    "SELECT run_id, status, finished_at FROM {ducklake_alias}.meta.runs ORDER BY started_at DESC LIMIT 1"
  ))
  n_snapshots <- DBI::dbGetQuery(con, glue::glue("SELECT COUNT(*) AS n FROM ducklake_snapshots('{ducklake_alias}')"))$n
  size_bytes <- sum(file.info(list.files(db, recursive = TRUE, full.names = TRUE))$size, na.rm = TRUE)

  tibble::tibble(
    n_surveys = surveys$n, n_surveys_deleted = surveys$n_deleted %||% 0L,
    n_responses = responses$n, n_responses_deleted = responses$n_deleted %||% 0L,
    n_snapshots = n_snapshots, size_bytes = size_bytes,
    last_run_id = if (nrow(last_run) > 0) last_run$run_id[1] else NA_character_,
    last_run_status = if (nrow(last_run) > 0) last_run$status[1] else NA_character_,
    last_run_finished_at = if (nrow(last_run) > 0) last_run$finished_at[1] else as.POSIXct(NA)
  )
}

#' Compact the application database
#'
#' Flushes small inlined commits to Parquet and merges adjacent files
#' (`ducklake_flush_inlined_data()`, `ducklake_merge_adjacent_files()`) --
#' the answer to "1-2 responses/day must not balloon storage" once those
#' tiny daily commits have accumulated (plan.md §2.2).
#'
#' @param db Application database directory. Defaults to [alchemer_db_path()].
#' @return Invisibly, a list with `flushed` and `merged` result tibbles.
#' @export
compact <- function(db = alchemer_db_path()) {
  con <- alchemer_db(db)
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  flushed <- DBI::dbGetQuery(con, glue::glue("SELECT * FROM ducklake_flush_inlined_data('{ducklake_alias}')"))
  merged <- DBI::dbGetQuery(con, glue::glue("SELECT * FROM ducklake_merge_adjacent_files('{ducklake_alias}')"))
  invisible(list(flushed = flushed, merged = merged))
}

#' Trim time-travel history
#'
#' Expires DuckLake snapshots older than `older_than` and reclaims their
#' underlying files. Time travel to an expired snapshot then fails cleanly
#' rather than silently returning wrong data -- history depth and retention
#' are the same knob (§8 of the spec).
#'
#' @param db Application database directory. Defaults to [alchemer_db_path()].
#' @param older_than A `Date`/`POSIXct` cutoff: snapshots committed before
#'   this are expired.
#' @return Invisibly, `TRUE`.
#' @export
expire_history <- function(db = alchemer_db_path(), older_than) {
  con <- alchemer_db(db)
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  DBI::dbExecute(con, glue::glue(
    "CALL ducklake_expire_snapshots('{ducklake_alias}', older_than => {DBI::dbQuoteLiteral(con, as.POSIXct(older_than))})"
  ))
  DBI::dbExecute(con, glue::glue("CALL ducklake_cleanup_old_files('{ducklake_alias}')"))
  invisible(TRUE)
}

#' Permanently remove research data and its history
#'
#' Unlike [expire_history()], which only trims how far back time travel
#' reaches, `expunge()` deletes the matching rows themselves and then
#' expires all history up to now, so the removed data cannot be recovered
#' through time travel either. Exactly one of `survey_id`/`before_date` must
#' be supplied.
#'
#' @param db Application database directory. Defaults to [alchemer_db_path()].
#' @param survey_id If supplied, every raw.* row for this survey (including
#'   its `raw.surveys` record) is removed.
#' @param before_date If supplied, `raw.responses` rows with
#'   `date_submitted` earlier than this are removed, across every survey.
#' @return Invisibly, `TRUE`.
#' @export
expunge <- function(db = alchemer_db_path(), survey_id = NULL, before_date = NULL) {
  if (is.null(survey_id) && is.null(before_date)) {
    cli::cli_abort("Supply survey_id or before_date.", class = "alchemeR_config_error")
  }
  con <- alchemer_db(db)
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))

  DBI::dbExecute(con, "BEGIN")
  tryCatch(
    {
      if (!is.null(survey_id)) {
        sid <- DBI::dbQuoteString(con, as.character(survey_id))
        for (tbl in c(
          "responses", "survey_definitions", "survey_pages", "survey_questions",
          "survey_question_options", "survey_statistics", "survey_campaigns"
        )) {
          DBI::dbExecute(con, glue::glue("DELETE FROM {ducklake_alias}.raw.{tbl} WHERE survey_id = {sid}"))
        }
        DBI::dbExecute(con, glue::glue("DELETE FROM {ducklake_alias}.raw.surveys WHERE survey_id = {sid}"))
        DBI::dbExecute(con, glue::glue("DELETE FROM {ducklake_alias}.meta.survey_state WHERE survey_id = {sid}"))
      }
      if (!is.null(before_date)) {
        cutoff <- DBI::dbQuoteLiteral(con, as.POSIXct(before_date))
        DBI::dbExecute(con, glue::glue(
          "DELETE FROM {ducklake_alias}.raw.responses
           WHERE TRY_CAST(regexp_replace(date_submitted, ' [A-Z]{{2,5}}$', '') AS TIMESTAMP) < {cutoff}"
        ))
      }
      DBI::dbExecute(con, "COMMIT")
    },
    error = function(e) {
      DBI::dbExecute(con, "ROLLBACK")
      cli::cli_abort("expunge() failed; no rows were removed.", parent = e, call = NULL)
    }
  )

  DBI::dbExecute(con, glue::glue("CALL ducklake_expire_snapshots('{ducklake_alias}', older_than => now())"))
  DBI::dbExecute(con, glue::glue("CALL ducklake_cleanup_old_files('{ducklake_alias}')"))
  invisible(TRUE)
}
