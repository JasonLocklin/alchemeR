# DuckLake has no primary key, unique, foreign key, or check constraints
# (ADR-006), so uniqueness and referential integrity are properties of the
# write pattern that must be asserted, not declared. These checks are shared
# by ingest() (which persists them to meta.integrity_checks after every
# survey commit) and db_check() (ad-hoc, read-only, no persistence).

# `expected_count`, when supplied, is the number of live responses the
# calling refresh believes it just fetched for `survey_id` -- it is only
# ever a *check*, never a filter (ADR-004's hint/selection distinction
# applies here too: getting `expected_count` wrong costs a failed
# assertion, never a silently wrong row count).
run_integrity_checks <- function(con, survey_id = NULL, expected_count = NULL) {
  scope_sql <- function(alias) {
    if (is.null(survey_id)) "" else glue::glue("WHERE {alias}.survey_id = {DBI::dbQuoteString(con, survey_id)}")
  }

  checks <- list()

  n_dupes <- DBI::dbGetQuery(con, glue::glue(
    "SELECT COUNT(*) AS n FROM (
       SELECT survey_id, response_id FROM {ducklake_alias}.raw.responses
       {scope_sql('responses')}
       GROUP BY survey_id, response_id HAVING COUNT(*) > 1
     )"
  ))$n
  checks$no_duplicate_responses <- list(
    passed = n_dupes == 0,
    message = if (n_dupes == 0) "OK" else glue::glue("{n_dupes} duplicate (survey_id, response_id) pair(s)")
  )

  if (!is.null(survey_id) && !is.null(expected_count)) {
    live_count <- DBI::dbGetQuery(con, glue::glue(
      "SELECT COUNT(*) AS n FROM {ducklake_alias}.raw.responses
       WHERE survey_id = {DBI::dbQuoteString(con, survey_id)}
         AND (is_deleted IS NULL OR NOT is_deleted)"
    ))$n
    checks$response_count_matches_fetch <- list(
      passed = live_count == expected_count,
      message = if (live_count == expected_count) {
        "OK"
      } else {
        glue::glue("{live_count} live rows stored, but the refresh reported fetching {expected_count}")
      }
    )
  }

  n_orphan_responses <- DBI::dbGetQuery(con, glue::glue(
    "SELECT COUNT(*) AS n FROM {ducklake_alias}.raw.responses r
     {scope_sql('r')}
     {if (is.null(survey_id)) 'WHERE' else 'AND'} NOT EXISTS (
       SELECT 1 FROM {ducklake_alias}.raw.surveys s WHERE s.survey_id = r.survey_id
     )"
  ))$n
  checks$responses_reference_known_surveys <- list(
    passed = n_orphan_responses == 0,
    message = if (n_orphan_responses == 0) "OK" else glue::glue("{n_orphan_responses} response(s) with no matching raw.surveys row")
  )

  n_orphan_options <- DBI::dbGetQuery(con, glue::glue(
    "SELECT COUNT(*) AS n FROM {ducklake_alias}.raw.survey_question_options o
     {scope_sql('o')}
     {if (is.null(survey_id)) 'WHERE' else 'AND'} NOT EXISTS (
       SELECT 1 FROM {ducklake_alias}.raw.survey_questions q
       WHERE q.survey_id = o.survey_id AND q.question_id = o.question_id
     )"
  ))$n
  checks$options_reference_known_questions <- list(
    passed = n_orphan_options == 0,
    message = if (n_orphan_options == 0) "OK" else glue::glue("{n_orphan_options} option(s) with no matching raw.survey_questions row")
  )

  tibble::tibble(
    survey_id = survey_id %||% NA_character_,
    check_name = names(checks),
    passed = purrr::map_lgl(checks, "passed"),
    message = purrr::map_chr(checks, "message")
  )
}

#' Run the application database's integrity assertions on demand
#'
#' Exposes the same checks `ingest()` runs after every survey commit
#' (ADR-006), for ad-hoc use -- e.g. after restoring a backup, or
#' investigating a data quality report.
#'
#' @param db Application database directory. Defaults to [alchemer_db_path()].
#' @return A tibble, one row per check (per survey, if `survey_id` is given).
#' @export
db_check <- function(db = alchemer_db_path()) {
  con <- alchemer_db(db, read_only = TRUE)
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  run_integrity_checks(con)
}
