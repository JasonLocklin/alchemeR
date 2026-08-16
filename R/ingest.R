# Full survey-level refresh, always (ADR-004). date_updated, total_count, and
# modified_on are used ONLY to decide *when* a survey is worth refreshing --
# never to select which rows to store. Every clause below can be wrong
# without ever producing a wrong or missing row: getting a hint wrong costs
# freshness (bounded by full_sweep_days), never correctness. Do not add a
# code path that uses these values to filter what gets written.

new_run_id <- function() {
  paste0(format(Sys.time(), "%Y%m%dT%H%M%OS3", tz = "UTC"), "Z-", random_suffix(6))
}

# The probe is one request per survey: the newest response by date_updated,
# plus the survey's total_count. Unlike alchemer_fetch(), this needs the
# envelope's sibling fields (total_count), not just `data`, so it calls the
# request/perform layer directly rather than going through unwrap_envelope().
probe_survey <- function(client, survey_id) {
  req <- alchemer_request(
    client, glue::glue("survey/{survey_id}/surveyresponse"),
    query = list(resultsperpage = 1, order_by = "-date_updated")
  )
  resp <- alchemer_perform(req, client)
  body <- httr2::resp_body_json(resp, simplifyVector = FALSE)
  if (isFALSE(body$result_ok)) {
    cli::cli_abort(
      c(
        "Probe request failed for survey {survey_id}.",
        "x" = body$message %||% "unknown error",
        "i" = "Code: {body$code %||% 'unknown'}"
      ),
      class = "alchemeR_api_result_error", code = body$code, call = NULL
    )
  }
  # body$data is an empty list for a survey with zero responses --
  # body$data[[1]] on that is "subscript out of bounds", not a missing
  # field, so it must be guarded explicitly rather than relying on %||%.
  newest <- if (length(body$data) > 0) body$data[[1]] else NULL
  list(
    total_count = as.integer(body$total_count %||% 0L),
    max_date_updated = chr1(newest$date_updated %||% NA)
  )
}

# Pure decision function, easy to unit test without a database or network.
# `prior` is one row of meta.survey_state (or NULL if the survey has never
# been seen). Every clause is a hint: getting one wrong costs freshness,
# never correctness (ADR-004).
decide_refresh <- function(prior, modified_on, probe, force, full_sweep_days, now = Sys.time()) {
  if (isTRUE(force)) {
    return(list(refresh = TRUE, reason = "force = TRUE"))
  }
  if (is.null(prior) || nrow(prior) == 0 || is.na(prior$last_successful_refresh_at[1])) {
    return(list(refresh = TRUE, reason = "never successfully refreshed"))
  }
  if (!identical(chr1(prior$last_modified_on[1]), chr1(modified_on))) {
    return(list(refresh = TRUE, reason = "survey definition changed (modified_on)"))
  }
  if (!identical(as.integer(prior$last_probe_total_count[1]), as.integer(probe$total_count)) ||
        !identical(chr1(prior$last_probe_max_date_updated[1]), chr1(probe$max_date_updated))) {
    return(list(refresh = TRUE, reason = "probe detected new, edited, or deleted responses"))
  }
  staleness_days <- as.numeric(difftime(now, prior$last_successful_refresh_at[1], units = "days"))
  if (staleness_days >= full_sweep_days) {
    return(list(
      refresh = TRUE,
      reason = glue::glue("full_sweep_days backstop ({round(staleness_days, 1)} days since last success)")
    ))
  }
  list(refresh = FALSE, reason = "no change detected")
}

get_survey_state <- function(con, survey_id) {
  DBI::dbGetQuery(con, glue::glue(
    "SELECT * FROM {ducklake_alias}.meta.survey_state WHERE survey_id = {DBI::dbQuoteString(con, survey_id)}"
  ))
}

# Replaces the one row for `survey_id`, computing consecutive_failures and
# last_successful_refresh_at from the prior state. Called both inside the
# survey's refresh transaction (on success) and, separately, in its own
# autocommit statement after a rollback (on failure) -- so a failure's
# bookkeeping survives the rollback that discards everything else (ADR-007's
# logic applies here too: failure records must outlive the failure).
#
# The three change-detection hints describe *the state that was successfully
# archived*, so a failed refresh must not advance them. Recording what a failed
# refresh saw made the next run compare those values against themselves,
# conclude "no change detected", and skip the survey -- so one failure silently
# parked it until full_sweep_days (90 days by default) elapsed or it happened
# to change again upstream, with consecutive_failures frozen at 1 and the
# responses it failed to fetch never archived. That is the ADR-004 rule read
# the wrong way round: a hint may only ever cost freshness, and this cost data.
# A failure has to leave change detection pointing at the last *successful*
# state, so that the difference the next run sees is what makes it retry.
update_survey_state <- function(con, survey_id, modified_on, probe, success, now = Sys.time()) {
  prior <- get_survey_state(con, survey_id)
  had_prior <- nrow(prior) > 0
  consecutive_failures <- if (!had_prior) 0L else as.integer(prior$consecutive_failures[1] %||% 0L)
  prior_success_at <- if (!had_prior) as.POSIXct(NA) else prior$last_successful_refresh_at[1]
  # With no prior row there is nothing to roll back to, and none is needed:
  # last_successful_refresh_at stays NA, which decide_refresh() short-circuits
  # to "never successfully refreshed" and retries on its own.
  keep_prior_hints <- !success && had_prior
  row <- tibble::tibble(
    survey_id = survey_id,
    last_modified_on = if (keep_prior_hints) chr1(prior$last_modified_on[1]) else chr1(modified_on),
    last_probe_total_count = if (keep_prior_hints) {
      as.integer(prior$last_probe_total_count[1])
    } else {
      as.integer(probe$total_count %||% NA_integer_)
    },
    last_probe_max_date_updated = if (keep_prior_hints) {
      chr1(prior$last_probe_max_date_updated[1])
    } else {
      chr1(probe$max_date_updated %||% NA)
    },
    # Not a hint: this records the attempt, which did happen. It is what
    # ingest_failures() reports as last_attempt_at.
    last_refresh_started_at = now,
    last_successful_refresh_at = if (success) now else prior_success_at,
    consecutive_failures = if (success) 0L else consecutive_failures + 1L
  )
  DBI::dbExecute(con, glue::glue(
    "DELETE FROM {ducklake_alias}.meta.survey_state WHERE survey_id = {DBI::dbQuoteString(con, survey_id)}"
  ))
  write_rows_generic(con, "meta.survey_state", row)
}

# Like write_rows() (db_schema.R), but for "meta.<table>" names, reordered
# against meta_table_columns for the same reason: INSERT ... SELECT * FROM
# <registered data.frame> maps columns by position, not name.
write_rows_generic <- function(con, table, rows) {
  if (nrow(rows) == 0) {
    return(invisible(NULL))
  }
  bare_name <- sub("^meta\\.", "", table)
  rows <- as.data.frame(dplyr::select(rows, dplyr::all_of(meta_table_columns[[bare_name]])))
  tmp_name <- paste0("tmp_write_", gsub("\\.", "_", table), "_", random_suffix())
  duckdb::duckdb_register(con, tmp_name, rows)
  DBI::dbExecute(con, glue::glue("INSERT INTO {ducklake_alias}.{table} SELECT * FROM {tmp_name}"))
  duckdb::duckdb_unregister(con, tmp_name)
  invisible(NULL)
}

# The status code a failure carries, for meta.run_events.http_status. Two
# different things land in that column, and they are not ambiguous in
# practice: the real HTTP status when the request itself failed
# (alchemeR_api_error), or Alchemer's own `code` when HTTP said 200 and the
# envelope said result_ok: false (alchemeR_api_result_error). Alchemer's codes
# are HTTP-shaped -- 401 for bad credentials, 500 for a server-side timeout --
# which is why one column can hold both and still mean something.
#
# NA is a real answer here (a connection failure never got a status, and an
# integrity-check failure never made a request), so this must not invent one.
error_status_code <- function(e) {
  # or_default(e$status, NULL) rather than e$status %||% e$code: a connection
  # failure sets `status` to NA rather than leaving it unset, and %||% only
  # falls through on NULL.
  status <- or_default(e$status, NULL) %||% e$code
  suppressWarnings(as.integer(status %||% NA_integer_))
}

# Every place a failure's text is shown -- meta.run_events.message, ingest()'s
# returned `message` column, the end-of-run warning -- is a place that renders
# one line: a tibble cell, a cron job's log file, a SQL client. An rlang or cli
# error is not one line. It arrives as a multi-line, glyph-bulleted block
# ("i In index: 1.\nCaused by error:\n! Result must be length 1, not 2."), and
# once that has been squeezed into a tibble cell what the reader gets is
# garbled -- the actual sentence truncated away behind the decoration.
#
# So flatten it here: drop the bullet glyphs, which only mean anything in the
# console they were formatted for, and join the lines. Every word survives;
# only the layout goes.
flatten_message <- function(x) {
  lines <- unlist(strsplit(as.character(x), "\n", fixed = TRUE))
  # cli's unicode bullets, plus the ASCII fallbacks it uses when the console
  # cannot render them. Anchored and space-terminated so a line of real text
  # that merely starts with "x" or "!" is left alone.
  glyphs <- "\u2139|\u2716|\u2714|\u2022|\u00d7|i|x|!|\\*|\\+"
  lines <- sub(paste0("^\\s*(", glyphs, ")\\s+"), "", lines)
  lines <- trimws(lines)
  paste(lines[nzchar(lines)], collapse = " ")
}

log_event <- function(con, run_id, survey_id, phase, status, http_status = NA_integer_,
                      message = NA_character_, started_at = Sys.time(), finished_at = Sys.time(),
                      n_responses = NA_integer_) {
  write_rows_generic(con, "meta.run_events", tibble::tibble(
    run_id = run_id, survey_id = chr1(survey_id), phase = phase, status = status,
    http_status = as.integer(http_status), message = as.character(message),
    started_at = started_at, finished_at = finished_at, n_responses = as.integer(n_responses)
  ))
}

# Surveys the discovery list no longer returns are flagged, never dropped
# (ADR-005 applies at survey level too). Not scoped to a survey: the whole
# account's discovery list is one set difference.
upsert_surveys <- function(con, run_id, discovered, now = Sys.time()) {
  discovered <- dplyr::mutate(
    discovered, ingested_at = now, run_id = run_id,
    is_deleted = FALSE, deleted_detected_at = as.POSIXct(NA)
  )
  merge_survey_rows(con, "surveys", NULL, discovered, on_vanished = "flag", now = now)
  invisible(discovered)
}

# Fetches everything for one survey and rebuilds its rows in every raw table
# inside a single transaction (one DuckLake snapshot per survey per run).
# Integrity is asserted before the transaction commits (ADR-006): a failure
# rolls the survey back, leaving its prior state exactly as it was. The check
# results themselves are recorded afterwards, pass or fail (ADR-007).
#
# `checks` and `n_fetched` are assigned inside the tryCatch() block below and
# read after it. That works because tryCatch() evaluates its expression in
# *this* frame, so a plain `<-` there is a local assignment here -- no `<<-`
# (which would reach past this frame entirely and leave both unset).
refresh_survey <- function(con, client, survey_id, run_id, include, modified_on, probe) {
  now <- Sys.time()
  # Stamps unconditionally, including on a zero-row frame: merge_survey_rows()
  # needs the full column set even when nothing was fetched, because that is
  # precisely the case where every stored row has vanished upstream.
  stamp <- function(df) {
    if (ncol(df) == 0) df else dplyr::mutate(df, ingested_at = now, run_id = run_id)
  }
  n_fetched <- NA_integer_
  checks <- NULL

  # BEGIN happens before any network call, and everything -- fetching,
  # parsing, and writing -- runs inside the one tryCatch, so a failure at
  # *any* point (a broken API response as much as a bad write) reaches the
  # same ROLLBACK + failure-bookkeeping path. Getting this wrong (e.g.
  # fetching before BEGIN) would silently skip consecutive_failures on a
  # fetch error, since the function would throw before ever reaching it.
  DBI::dbExecute(con, "BEGIN")
  result <- tryCatch(
    {
      def <- alchemer_fetch(client, glue::glue("survey/{survey_id}"))
      varnames <- parse_varnames(alchemer_fetch_all(client, glue::glue("survey/{survey_id}/surveyquestion")))
      parsed_def <- parse_survey_definition(survey_id, def, varnames)

      response_items <- alchemer_fetch_all(client, glue::glue("survey/{survey_id}/surveyresponse"))
      fetched_responses <- parse_responses(survey_id, response_items)
      n_fetched <- nrow(fetched_responses)
      responses <- stamp(dplyr::mutate(
        fetched_responses, is_deleted = FALSE, deleted_detected_at = as.POSIXct(NA)
      ))

      merge_survey_rows(con, "survey_definitions", survey_id, stamp(parsed_def$definition))
      merge_survey_rows(con, "survey_pages", survey_id, stamp(parsed_def$pages))
      merge_survey_rows(con, "survey_questions", survey_id, stamp(parsed_def$questions))
      merge_survey_rows(con, "survey_question_options", survey_id, stamp(parsed_def$options))
      merge_survey_rows(con, "responses", survey_id, responses, on_vanished = "flag", now = now)

      # A dataset left out of `include` is skipped entirely, not merged as an
      # empty set: an empty fetch means "every stored row has vanished
      # upstream", which would delete whatever an earlier run archived.
      # Omitting a dataset means "don't fetch it this time", never "delete it".
      if ("campaigns" %in% include) {
        merge_survey_rows(con, "survey_campaigns", survey_id, stamp(parse_campaigns(
          survey_id, alchemer_fetch_all(client, glue::glue("survey/{survey_id}/surveycampaign"))
        )))
      }
      if ("statistics" %in% include) {
        merge_survey_rows(con, "survey_statistics", survey_id, stamp(parse_statistics(
          survey_id, alchemer_fetch_all(client, glue::glue("survey/{survey_id}/surveystatistic"))
        )))
      }

      checks <- run_integrity_checks(con, survey_id = survey_id, expected_count = n_fetched)
      failed <- checks[!checks$passed, , drop = FALSE]
      if (nrow(failed) > 0) {
        stop(paste("integrity check(s) failed:", paste(failed$message, collapse = "; ")), call. = FALSE)
      }

      update_survey_state(con, survey_id, modified_on, probe, success = TRUE, now = now)
      list(status = "ok", message = "OK", http_status = NA_integer_)
    },
    # conditionMessage(), not the bare condition: rlang formats a cli_abort's
    # bullets into the message, so the redacted path, the API's own code, and
    # the failing check names all survive into meta.run_events.message. The
    # status code is the one part that does not, so it is pulled out here.
    error = function(e) {
      list(
        status = "error", message = flatten_message(conditionMessage(e)),
        http_status = error_status_code(e)
      )
    }
  )

  if (identical(result$status, "ok")) {
    DBI::dbExecute(con, "COMMIT")
  } else {
    DBI::dbExecute(con, "ROLLBACK")
    update_survey_state(con, survey_id, modified_on, probe, success = FALSE, now = now)
  }

  # Written after the transaction resolves, never inside it (ADR-007): a
  # failing check used to abort before this line *and* would have been rolled
  # back anyway, so meta.integrity_checks could only ever contain passes --
  # exactly inverting its value for monitoring, since "no failed rows" did not
  # mean "no failed checks".
  if (!is.null(checks)) {
    write_rows_generic(con, "meta.integrity_checks", dplyr::mutate(checks, run_id = run_id, checked_at = now))
  }

  c(result, list(n_fetched = n_fetched))
}

#' Ingest the whole Alchemer account into the local application database
#'
#' Downloads every survey's full definition and **all** of its responses
#' (never a filtered subset -- ADR-004) and rebuilds each survey's rows in
#' one transaction, so a failure partway through never leaves partial data.
#' Change detection (`date_updated`, `modified_on`, `total_count`) decides
#' only *when* a survey is worth refreshing, and is designed to be safely
#' wrong: an incorrect hint costs freshness, bounded by `full_sweep_days`,
#' never a missing or wrong row.
#'
#' @param db Application database directory. Defaults to [alchemer_db_path()].
#' @param surveys If supplied, only these survey ids are considered, and are
#'   always refreshed regardless of change detection.
#' @param force Refresh every survey regardless of change detection.
#' @param include Which optional per-survey data to fetch: any of
#'   `"campaigns"` and `"statistics"`, both on by default. Contacts (heavy
#'   PII, ADR-009) are account-level rather than per-survey and are not
#'   fetched by `ingest()` at all yet.
#' @param dry_run If `TRUE`, make the same decisions and requests but write no
#'   data. The database directory and its empty tables are still created if
#'   they don't exist yet, so a dry run against a fresh `ALCHEMER_DB` leaves a
#'   valid, empty database behind.
#' @param client An `alchemer_client`. Defaults to one built from the
#'   environment; tests pass a fixture-backed client (ADR-010).
#' @return Invisibly, a tibble with one row per survey considered:
#'   `survey_id`, the `decision` that was made and whether it led to a
#'   `refresh`ed survey, `n_fetched`, a `status` of `"ok"`/`"skipped"`/
#'   `"dry_run"`/`"error"`, the failure `message` and `http_status` when that
#'   status is `"error"`, and timings.
#' @section Configuration:
#' Credentials, the API domain, the request throttle, and the staleness
#' backstop all come from the environment and cannot be passed here (ADR-019):
#' `ALCHEMER_API_TOKEN`, `ALCHEMER_API_SECRET`, `ALCHEMER_DOMAIN`,
#' `ALCHEMER_RPM`, and `ALCHEMER_FULL_SWEEP_DAYS`. See
#' `vignette("getting-started")`.
#' @section When a survey fails:
#' A failure is scoped to one survey. Its refresh is rolled back whole, so the
#' data already archived for it is left exactly as it was, every other survey
#' still commits, and the next run tries again -- there is nothing to clean up
#' and no partial state to reconcile.
#'
#' `ingest()` warns when any survey failed, and the reason is on the returned
#' tibble. It is also persisted: see [ingest_failures()] for the surveys
#' currently failing and why, and `vignette("troubleshooting")` for how to read
#' a particular message.
#' @seealso [ingest_failures()], [db_status()], [db_check()]
#' @export
ingest <- function(db = alchemer_db_path(), surveys = NULL, force = FALSE,
                   include = c("campaigns", "statistics"),
                   dry_run = FALSE, client = alchemer_client()) {
  full_sweep_days <- alchemer_full_sweep_days()
  run_id <- new_run_id()
  run_started_at <- Sys.time()
  con <- alchemer_db(db)
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  # Before the run is logged, let alone before a request is made: writing to an
  # archive whose layout this code doesn't understand is the one failure that
  # can't be undone by running again (ADR-018).
  assert_schema_compatible(con, db)

  if (!dry_run) {
    write_rows_generic(con, "meta.runs", tibble::tibble(
      run_id = run_id, started_at = run_started_at, finished_at = as.POSIXct(NA),
      status = "running", package_version = as.character(utils::packageVersion("alchemeR")),
      duckdb_version = as.character(utils::packageVersion("duckdb")),
      n_checked = NA_integer_, n_refreshed = NA_integer_, n_failed = NA_integer_, n_requests = NA_integer_
    ))
  }

  discovered <- parse_surveys(alchemer_fetch_all(client, "survey"))
  if (!dry_run) {
    discovered <- upsert_surveys(con, run_id, discovered, run_started_at)
  }
  is_deleted_col <- if ("is_deleted" %in% names(discovered)) discovered$is_deleted else rep(FALSE, nrow(discovered))
  candidate_ids <- if (!is.null(surveys)) as.character(surveys) else discovered$survey_id[!is_deleted_col %in% TRUE]

  results <- purrr::map(candidate_ids, function(survey_id) {
    t0 <- Sys.time()
    modified_on <- discovered$modified_on[discovered$survey_id == survey_id][1]
    prior_state <- if (dry_run) NULL else get_survey_state(con, survey_id)

    # A failed probe is not a failed refresh -- the survey is refreshed anyway,
    # and usually succeeds -- so it must not abort anything. It is still worth
    # recording: the error used to be swallowed whole, which left "probe
    # request failed" as the only trace of, say, a survey the API consistently
    # 500s on. Kept as a condition object rather than a message so the status
    # code survives to the log too.
    probed <- tryCatch(
      list(probe = probe_survey(client, survey_id), error = NULL),
      error = function(e) list(probe = NULL, error = e)
    )
    probe <- probed$probe

    decision <- if (!is.null(surveys)) {
      list(refresh = TRUE, reason = "explicit surveys= argument")
    } else if (is.null(probe)) {
      list(refresh = TRUE, reason = "probe request failed; refreshing to be safe")
    } else {
      decide_refresh(prior_state, modified_on, probe, force, full_sweep_days)
    }

    if (!dry_run) {
      if (!is.null(probed$error)) {
        log_event(
          con, run_id, survey_id, "probe", "error",
          http_status = error_status_code(probed$error),
          message = flatten_message(conditionMessage(probed$error)),
          started_at = t0, finished_at = Sys.time()
        )
      }
      log_event(
        con, run_id, survey_id, "decision", "info",
        message = decision$reason, started_at = t0, finished_at = Sys.time()
      )
    }

    if (!decision$refresh) {
      return(tibble::tibble(
        survey_id = survey_id, decision = decision$reason, refreshed = FALSE,
        n_fetched = NA_integer_, status = "skipped", message = NA_character_,
        http_status = NA_integer_, started_at = t0, finished_at = Sys.time()
      ))
    }
    if (dry_run) {
      return(tibble::tibble(
        survey_id = survey_id, decision = decision$reason, refreshed = TRUE,
        n_fetched = NA_integer_, status = "dry_run", message = NA_character_,
        http_status = NA_integer_, started_at = t0, finished_at = Sys.time()
      ))
    }

    probe_or_unknown <- probe %||% list(total_count = NA_integer_, max_date_updated = NA_character_)
    outcome <- tryCatch(
      refresh_survey(con, client, survey_id, run_id, include, modified_on, probe_or_unknown),
      error = function(e) {
        list(
          status = "error", message = flatten_message(conditionMessage(e)),
          http_status = error_status_code(e), n_fetched = NA_integer_
        )
      }
    )
    log_event(
      con, run_id, survey_id, "refresh", outcome$status, http_status = outcome$http_status,
      message = outcome$message, started_at = t0, finished_at = Sys.time(),
      n_responses = outcome$n_fetched
    )

    # `message` is carried out of ingest() as well as into meta.run_events:
    # a caller that has just watched two surveys fail should not have to open
    # the database to find out why (inst/scripts/ingest_job.R prints it).
    tibble::tibble(
      survey_id = survey_id, decision = decision$reason, refreshed = TRUE,
      n_fetched = outcome$n_fetched, status = outcome$status, message = outcome$message,
      http_status = outcome$http_status, started_at = t0, finished_at = Sys.time()
    )
  })

  # A bare tibble::tibble() here (no columns) would make every out$status/
  # out$refreshed access below a silent "unknown column" warning returning
  # NULL rather than the empty-but-typed vector expected -- same class of
  # issue as the empty-parse-result columns fixed in parse.R.
  empty_out <- tibble::tibble(
    survey_id = character(0), decision = character(0), refreshed = logical(0),
    n_fetched = integer(0), status = character(0), message = character(0),
    http_status = integer(0), started_at = Sys.time()[0], finished_at = Sys.time()[0]
  )
  out <- if (length(results) == 0) empty_out else dplyr::bind_rows(results)

  if (!dry_run) {
    write_rows_generic(con, "meta.run_events", tibble::tibble(
      run_id = run_id, survey_id = NA_character_, phase = "run", status = "finished",
      http_status = NA_integer_, message = NA_character_,
      started_at = run_started_at, finished_at = Sys.time(), n_responses = NA_integer_
    ))
    DBI::dbExecute(con, glue::glue(
      "DELETE FROM {ducklake_alias}.meta.runs WHERE run_id = {DBI::dbQuoteString(con, run_id)}"
    ))
    write_rows_generic(con, "meta.runs", tibble::tibble(
      run_id = run_id, started_at = run_started_at, finished_at = Sys.time(),
      status = if (nrow(out) > 0 && any(out$status == "error")) "completed_with_errors" else "completed",
      package_version = as.character(utils::packageVersion("alchemeR")),
      duckdb_version = as.character(utils::packageVersion("duckdb")),
      n_checked = nrow(out), n_refreshed = sum(out$refreshed %in% TRUE),
      n_failed = sum(out$status == "error"), n_requests = requests_made(client)
    ))
  }

  # The return value is invisible, so without this a scheduled run -- and an
  # interactive one -- reported a survey failing by saying nothing at all.
  # Warn once for the run rather than per survey: an account-wide problem
  # (expired key, wrong domain) fails every survey, and one line per survey
  # would bury the signal it is trying to raise.
  failed <- out[out$status == "error", , drop = FALSE]
  if (nrow(failed) > 0) {
    cli::cli_warn(c(
      "{nrow(failed)} of {nrow(out)} survey{?s} failed to refresh: {.val {failed$survey_id}}.",
      "i" = "Each survey's prior data is untouched; a failure rolls its refresh back whole.",
      "i" = "Why, per survey: the {.field message} column of this call's return value.",
      "i" = "Later: {.run alchemeR::ingest_failures()}, or {.code vignette(\"troubleshooting\")}."
    ), class = "alchemeR_ingest_failures")
  }

  invisible(out)
}
