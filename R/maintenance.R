# Time-travel depth and expungement share one knob (§8 of the spec):
# history only reaches as far back as retention allows, and expire_history()
# is what trims it. expunge() goes further -- it removes rows *and* their
# history, for research data that must not merely become invisible but
# actually leave the database.

#' Application database summary
#'
#' Covers both stages: the most recent `ingest()` run and the most recent
#' [load_pub_layer()]. [load_pipeline_health()] copies this row into the
#' analytics database, so a monitor watching that table can tell a stalled
#' extract from a stalled load.
#'
#' @param db Application database directory. Defaults to [alchemer_db_path()].
#' @return A one-row tibble: survey/response counts (live and flagged
#'   deleted), snapshot count, on-disk size, and the most recent ingest run
#'   and load.
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
  last_load <- if (table_exists(con, "meta", "loads")) {
    DBI::dbGetQuery(con, glue::glue(
      "SELECT load_id, status, finished_at, n_tables, n_rows FROM {ducklake_alias}.meta.loads
       ORDER BY started_at DESC LIMIT 1"
    ))
  } else {
    data.frame()
  }
  n_snapshots <- DBI::dbGetQuery(con, glue::glue("SELECT COUNT(*) AS n FROM ducklake_snapshots('{ducklake_alias}')"))$n
  size_bytes <- sum(file.info(list.files(db, recursive = TRUE, full.names = TRUE))$size, na.rm = TRUE)
  had_load <- nrow(last_load) > 0

  tibble::tibble(
    n_surveys = surveys$n, n_surveys_deleted = or_default(surveys$n_deleted, 0L),
    n_responses = responses$n, n_responses_deleted = or_default(responses$n_deleted, 0L),
    n_snapshots = n_snapshots, size_bytes = size_bytes,
    last_run_id = if (nrow(last_run) > 0) last_run$run_id[1] else NA_character_,
    last_run_status = if (nrow(last_run) > 0) last_run$status[1] else NA_character_,
    last_run_finished_at = if (nrow(last_run) > 0) last_run$finished_at[1] else as.POSIXct(NA),
    last_load_id = if (had_load) last_load$load_id[1] else NA_character_,
    last_load_status = if (had_load) last_load$status[1] else NA_character_,
    last_load_finished_at = if (had_load) last_load$finished_at[1] else as.POSIXct(NA),
    last_load_tables = if (had_load) as.integer(last_load$n_tables[1]) else NA_integer_,
    last_load_rows = if (had_load) as.integer(last_load$n_rows[1]) else NA_integer_
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
  cutoff <- DBI::dbQuoteLiteral(con, as.POSIXct(older_than))
  DBI::dbExecute(con, glue::glue(
    "CALL ducklake_expire_snapshots('{ducklake_alias}', older_than => {cutoff})"
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
#'   Interpreted as a calendar date in `tz` (midnight local time);
#'   `date_submitted` itself is parsed using its own per-row EST/EDT suffix,
#'   so the comparison is DST-correct without guessing at a fixed offset.
#' @param tz Timezone `before_date` is read in. Defaults to `ALCHEMER_TZ`,
#'   then the machine's own timezone (see [alchemer_tz()]).
#' @details
#' The matching `pub` rows go too. `pub.responses`/`pub.answers` hold a
#' *second copy* of the answer text (`pub.answers.answer` is the verbatim
#' string), so deleting only from `raw` would leave the expunged data fully
#' readable -- and, worse, would leave it as the only remaining copy once
#' history is expired, then push it downstream on the next
#' [load_pub_layer()] run.
#' @return Invisibly, `TRUE`.
#' @export
expunge <- function(db = alchemer_db_path(), survey_id = NULL, before_date = NULL,
                    tz = NULL) {
  tz <- alchemer_tz(tz) # validated even when supplied: it reaches SQL as a literal
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
        for (tbl in c("surveys", "questions", "options", "responses", "answers")) {
          DBI::dbExecute(con, glue::glue("DELETE FROM {ducklake_alias}.pub.{tbl} WHERE survey_id = {sid}"))
        }
      }
      if (!is.null(before_date)) {
        # date_submitted carries an explicit EST or EDT suffix per row --
        # the correct abbreviation for that specific date -- so it parses
        # into an exact instant with no DST calendar lookup of our own,
        # just by honoring the offset Alchemer already told us (EST/EDT
        # are always -05:00/-04:00; there is no ambiguity to resolve).
        # before_date is interpreted as midnight in `tz`, matching how a
        # retention cutoff is actually read and set.
        cutoff <- glue::glue(
          "(TIMESTAMP {DBI::dbQuoteString(con, format(as.Date(before_date)))}",
          " AT TIME ZONE {DBI::dbQuoteString(con, tz)})"
        )
        DBI::dbExecute(con, glue::glue(
          "DELETE FROM {ducklake_alias}.raw.responses
           WHERE (CASE
             WHEN date_submitted LIKE '% EST' THEN
               TRY_CAST(regexp_replace(date_submitted, ' EST$', '') || '-05' AS TIMESTAMPTZ)
             WHEN date_submitted LIKE '% EDT' THEN
               TRY_CAST(regexp_replace(date_submitted, ' EDT$', '') || '-04' AS TIMESTAMPTZ)
             ELSE NULL
           END) < {cutoff}"
        ))
      }
      # Whatever the criterion was, pub.responses/pub.answers must not keep
      # rows whose raw.responses row has just gone: pub.answers.answer is a
      # verbatim copy of the answer text, so leaving it behind would defeat
      # the whole point of expunging. Keyed off raw rather than off the
      # criterion so both branches above are covered by one rule.
      for (tbl in c("responses", "answers")) {
        DBI::dbExecute(con, glue::glue(
          "DELETE FROM {ducklake_alias}.pub.{tbl} p WHERE NOT EXISTS (
             SELECT 1 FROM {ducklake_alias}.raw.responses r
             WHERE r.survey_id = p.survey_id AND r.response_id = p.response_id
           )"
        ))
      }
      DBI::dbExecute(con, "COMMIT")
    },
    error = function(e) {
      DBI::dbExecute(con, "ROLLBACK")
      cli::cli_abort("expunge() failed; no rows were removed.", parent = e, call = NULL)
    }
  )

  # After the commit, not inside it: dropping a view is DDL, and a failure
  # here would leave an empty view behind (harmless) rather than roll back
  # deletes that have already succeeded.
  if (!is.null(survey_id)) {
    drop_existing_wide_views(con, as.character(survey_id))
  }

  DBI::dbExecute(con, glue::glue("CALL ducklake_expire_snapshots('{ducklake_alias}', older_than => now())"))
  DBI::dbExecute(con, glue::glue("CALL ducklake_cleanup_old_files('{ducklake_alias}')"))
  invisible(TRUE)
}
