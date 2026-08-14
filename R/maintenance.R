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

#' Surveys that are currently failing to refresh, and why
#'
#' The diagnostic companion to [db_status()], which reports only that the last
#' run *had* failures. This names them: one row per survey whose most recent
#' [ingest()] attempt failed, carrying the error message, the status code, and
#' any failed integrity checks from that same attempt.
#'
#' @param db Application database directory. Defaults to [alchemer_db_path()].
#' @return A tibble, one row per currently-failing survey (zero rows when
#'   nothing is failing), ordered worst-first by `consecutive_failures`:
#'
#'   \describe{
#'     \item{`survey_id`, `title`}{Which survey. `title` is `NA` for a survey
#'       that has never been archived successfully.}
#'     \item{`consecutive_failures`}{Failed attempts since the last success.
#'       Climbing across runs means a persistent cause, not a blip.}
#'     \item{`last_successful_refresh_at`}{How stale the archived data is --
#'       `NA` if this survey has never succeeded. **The data as of this
#'       timestamp is still intact**; a failed refresh never damages it.}
#'     \item{`last_attempt_at`, `last_error_run_id`, `last_error_phase`}{When
#'       it last tried, under which `meta.runs` id, and at which phase
#'       (`"refresh"`, or `"probe"` for a change-detection request that failed
#'       without stopping the refresh).}
#'     \item{`http_status`}{The HTTP status when the request itself failed, or
#'       Alchemer's own error code when it answered 200 with
#'       `result_ok: false` -- Alchemer's codes are HTTP-shaped (401, 500), so
#'       both read the same way. `NA` for a connection failure, which never got
#'       a status, or an integrity failure, which made no request.}
#'     \item{`message`}{The full error, including the redacted request path.
#'       `vignette("troubleshooting")` decodes the recurring shapes.}
#'     \item{`failed_checks`}{Integrity assertions that failed on that attempt
#'       (ADR-006), `check_name: message` and semicolon-separated. Populated
#'       only when the failure *was* an integrity failure.}
#'   }
#' @details
#' Reads `meta.survey_state` -- written outside the refresh transaction
#' precisely so a failure's bookkeeping survives its own rollback (ADR-007) --
#' joined to the matching `meta.run_events` and `meta.integrity_checks` rows.
#' Those tables remain the complete log; this is the standing question asked of
#' them. Notably, a survey listed here is *failing now*: it drops off by itself
#' on the next successful refresh.
#'
#' A survey whose probe fails but whose refresh then succeeds is **not** listed,
#' because it is not failing -- change detection just fell back to refreshing it
#' anyway. Those events are in `meta.run_events` with `phase = 'probe'`.
#' @seealso [db_status()] for the run-level summary, [db_check()] to re-run the
#'   integrity assertions account-wide.
#' @export
ingest_failures <- function(db = alchemer_db_path()) {
  con <- alchemer_db(db, read_only = TRUE)
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))

  tibble::as_tibble(DBI::dbGetQuery(con, glue::glue(
    "WITH failing AS (
       SELECT survey_id, consecutive_failures,
              last_refresh_started_at, last_successful_refresh_at
       FROM {ducklake_alias}.meta.survey_state
       WHERE consecutive_failures > 0 OR last_successful_refresh_at IS NULL
     ),
     errors AS (
       SELECT survey_id, run_id, phase, http_status, message,
              ROW_NUMBER() OVER (PARTITION BY survey_id ORDER BY finished_at DESC) AS rn
       FROM {ducklake_alias}.meta.run_events
       WHERE status = 'error' AND survey_id IS NOT NULL
     ),
     checks AS (
       SELECT run_id, survey_id,
              string_agg(check_name || ': ' || COALESCE(message, ''), '; ') AS failed_checks
       FROM {ducklake_alias}.meta.integrity_checks
       WHERE NOT passed AND survey_id IS NOT NULL
       GROUP BY run_id, survey_id
     )
     SELECT f.survey_id, s.title, f.consecutive_failures,
            f.last_successful_refresh_at, f.last_refresh_started_at AS last_attempt_at,
            e.run_id AS last_error_run_id, e.phase AS last_error_phase,
            e.http_status, e.message, c.failed_checks
     FROM failing f
     LEFT JOIN {ducklake_alias}.raw.surveys s ON s.survey_id = f.survey_id
     LEFT JOIN errors e ON e.survey_id = f.survey_id AND e.rn = 1
     LEFT JOIN checks c ON c.survey_id = e.survey_id AND c.run_id = e.run_id
     ORDER BY f.consecutive_failures DESC, f.survey_id"
  )))
}

#' Compact the application database
#'
#' Flushes inlined commits to Parquet, merges the small files repeated
#' refreshes leave behind, and rewrites files that have accumulated enough
#' delete markers to slow reads.
#'
#' @param db Application database directory. Defaults to [alchemer_db_path()].
#' @return Invisibly, a list with `flushed`, `merged`, and `rewritten` results.
#' @details
#' Run this on a schedule -- weekly is usually plenty. Each refresh writes a
#' small Parquet file per survey that changed, so an active account accumulates
#' many small files, and DuckLake's own guidance is that files should be at
#' least a few megabytes. *Measured*: 200 small files compacted to one.
#'
#' Deliberately does **not** expire snapshots, so it can be scheduled freely
#' without shortening how far back time travel reaches -- that is
#' [expire_history()]'s job. (DuckDB's `CHECKPOINT` statement bundles both,
#' which is why this calls the individual functions instead.)
#' @export
compact <- function(db = alchemer_db_path()) {
  con <- alchemer_db(db)
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  flushed <- DBI::dbGetQuery(con, glue::glue("SELECT * FROM ducklake_flush_inlined_data('{ducklake_alias}')"))
  merged <- DBI::dbGetQuery(con, glue::glue("SELECT * FROM ducklake_merge_adjacent_files('{ducklake_alias}')"))
  # Updates are delete-plus-insert underneath, so files accumulate delete
  # markers that slow reads until the file is rewritten without them. Left at
  # DuckLake's default threshold (rewrite once 95% of a file is deleted);
  # expunge() drops the threshold to 0 because it needs every deleted row
  # physically gone, not just most of them.
  rewritten <- rewrite_deleted_files(con)
  invisible(list(flushed = flushed, merged = merged, rewritten = rewritten))
}

# `CALL`, wrapped in an explicit transaction: the table-function form
# (`SELECT * FROM ducklake_rewrite_data_files(...)`) fails with "Scanning a
# DuckLake table after the transaction has ended", and the bare CALL hits the
# same error outside a transaction.
rewrite_deleted_files <- function(con, delete_threshold = NULL) {
  threshold <- if (is.null(delete_threshold)) "" else glue::glue(", delete_threshold => {delete_threshold}")
  DBI::dbExecute(con, "BEGIN")
  out <- tryCatch(
    {
      DBI::dbExecute(con, glue::glue("CALL ducklake_rewrite_data_files('{ducklake_alias}'{threshold})"))
      DBI::dbExecute(con, "COMMIT")
      TRUE
    },
    error = function(e) {
      DBI::dbExecute(con, "ROLLBACK")
      cli::cli_warn(c("Could not rewrite deleted data files.", "x" = conditionMessage(e)))
      FALSE
    }
  )
  invisible(out)
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
  cleanup_files(con)
  invisible(TRUE)
}

# `cleanup_all => true` matters: without it, cleanup_old_files() honours a grace
# period for files that might still be open elsewhere and reclaims nothing.
# *Measured*: 21 files before, 21 after without the flag, 1 with it. This
# function claims to reclaim the files it expires, so it has to pass it.
cleanup_files <- function(con) {
  DBI::dbExecute(con, glue::glue(
    "CALL ducklake_cleanup_old_files('{ducklake_alias}', cleanup_all => true)"
  ))
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
#' Removing the rows is only the first of four steps, because a `DELETE` alone
#' leaves the values recoverable from disk in three separate places. Each was
#' verified by searching the raw bytes of the database directory for a known
#' answer string:
#'
#' 1. **Inlined rows.** A small write goes into the catalog rather than into a
#'    Parquet file, and deleting an inlined row only stamps its `end_snapshot`
#'    -- the row and its values stay in the catalog table. So the inlined data
#'    is flushed to Parquet *before* anything is deleted.
#' 2. **The matching `pub` rows.** `pub.answers.answer` is a verbatim copy of
#'    the answer text, so deleting only from `raw` leaves the data fully
#'    readable by SQL -- and leaves it as the *only* remaining copy once
#'    history is expired, ready to be pushed downstream by the next
#'    [load_pub_layer()].
#' 3. **The Parquet files.** DuckLake deletes are merge-on-read: the row stays
#'    in its data file behind a delete marker. A file that still holds
#'    surviving rows is not removed by expiry, so it must be rewritten without
#'    the deleted rows (`ducklake_rewrite_data_files(delete_threshold => 0)`).
#' 4. **The catalog's statistics.** DuckLake records per-file **verbatim**
#'    minimum and maximum values for every column, `survey_data` included --
#'    so a response's answer JSON can sit in the catalog as a statistic.
#'    Deleting the statistic is not enough either: SQLite leaves freed pages in
#'    place until the file is `VACUUM`ed.
#'
#' Step 3 needs the `RSQLite` package. DuckDB's own `sqlite` extension cannot
#' run `VACUUM` (it accepts the statement but does not rebuild the file, and
#' its `sqlite_query()` pass-through rejects statements that return no rows).
#' @return Invisibly, `TRUE`.
#' @export
expunge <- function(db = alchemer_db_path(), survey_id = NULL, before_date = NULL,
                    tz = NULL) {
  tz <- alchemer_tz(tz) # validated even when supplied: it reaches SQL as a literal
  if (is.null(survey_id) && is.null(before_date)) {
    cli::cli_abort("Supply survey_id or before_date.", class = "alchemeR_config_error")
  }
  # Checked before anything is deleted, not after: a half-expunged database --
  # rows gone from SQL but values still recoverable from the catalog -- is the
  # worst possible outcome for a function whose whole purpose is that they are
  # not recoverable.
  rlang::check_installed(
    "RSQLite",
    "to vacuum the catalog so expunged values cannot be recovered from it."
  )
  con <- alchemer_db(db)
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))

  # Step 1 of the four in @details, and it has to come *before* the deletes:
  # a small write is inlined into the catalog rather than written to Parquet,
  # and deleting an inlined row only stamps its end_snapshot -- the row, values
  # and all, stays in the catalog table. Flushing first moves everything into
  # Parquet files, where a delete can actually be made to remove it.
  DBI::dbExecute(con, glue::glue("CALL ducklake_flush_inlined_data('{ducklake_alias}')"))

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

  # Step 3 of the four in @details: rewrite every file that now holds a delete
  # marker, at threshold 0 so a single expunged row is enough to warrant it.
  # Without this the values stay in their Parquet file -- unreadable by SQL,
  # but plainly there in the bytes.
  rewrite_deleted_files(con, delete_threshold = 0)
  DBI::dbExecute(con, glue::glue("CALL ducklake_expire_snapshots('{ducklake_alias}', older_than => now())"))
  cleanup_files(con)

  # Step 4: the catalog. Must run with no connection attached to it, so the
  # DuckLake connection is closed first rather than at on.exit.
  DBI::dbDisconnect(con, shutdown = TRUE)
  on.exit()
  vacuum_catalog(db)
  invisible(TRUE)
}

# Rebuilds catalog.sqlite so freed pages -- which can still hold verbatim
# column statistics for expunged rows -- are gone from the file.
vacuum_catalog <- function(db) {
  catalog <- file.path(normalizePath(db, mustWork = FALSE), "catalog.sqlite")
  con <- DBI::dbConnect(RSQLite::SQLite(), catalog)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbExecute(con, "VACUUM")
  invisible(TRUE)
}
