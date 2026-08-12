# Full survey-level refresh, always (ADR-004). date_updated, total_count, and
# modified_on are used ONLY to decide *when* a survey is worth refreshing --
# never to select which rows to store. Every clause below can be wrong
# without ever producing a wrong or missing row: getting a hint wrong costs
# freshness (bounded by full_sweep_days), never correctness. Do not add a
# code path that uses these values to filter what gets written.

new_run_id <- function() {
  paste0(format(Sys.time(), "%Y%m%dT%H%M%OS3", tz = "UTC"), "Z-", paste(sample(letters, 6), collapse = ""))
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
  resp <- alchemer_perform(req)
  body <- httr2::resp_body_json(resp, simplifyVector = FALSE)
  if (isFALSE(body$result_ok)) {
    cli::cli_abort(
      c("Probe request failed for survey {survey_id}.", "x" = body$message %||% "unknown error"),
      class = "alchemeR_api_result_error", call = NULL
    )
  }
  list(
    total_count = as.integer(body$total_count %||% 0L),
    max_date_updated = chr1(body$data[[1]]$date_updated %||% NA)
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
update_survey_state <- function(con, survey_id, modified_on, probe, success, now = Sys.time()) {
  prior <- get_survey_state(con, survey_id)
  consecutive_failures <- if (nrow(prior) == 0) 0L else as.integer(prior$consecutive_failures[1] %||% 0L)
  row <- tibble::tibble(
    survey_id = survey_id,
    last_modified_on = chr1(modified_on),
    last_probe_total_count = as.integer(probe$total_count %||% NA_integer_),
    last_probe_max_date_updated = chr1(probe$max_date_updated %||% NA),
    last_refresh_started_at = now,
    last_successful_refresh_at = if (success) now else (if (nrow(prior) == 0) as.POSIXct(NA) else prior$last_successful_refresh_at[1]),
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
  tmp_name <- paste0("tmp_write_", gsub("\\.", "_", table), "_", paste(sample(letters, 12, replace = TRUE), collapse = ""))
  duckdb::duckdb_register(con, tmp_name, rows)
  DBI::dbExecute(con, glue::glue("INSERT INTO {ducklake_alias}.{table} SELECT * FROM {tmp_name}"))
  duckdb::duckdb_unregister(con, tmp_name)
  invisible(NULL)
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
# (ADR-005 applies at survey level too). raw.surveys is small (<=~100 rows
# per the spec's stated scale), so it is replaced wholesale each run rather
# than diffed row by row.
upsert_surveys <- function(con, run_id, discovered, now = Sys.time()) {
  prior <- DBI::dbGetQuery(con, glue::glue("SELECT * FROM {ducklake_alias}.raw.surveys"))
  discovered <- dplyr::mutate(discovered, ingested_at = now, run_id = run_id, is_deleted = FALSE, deleted_detected_at = as.POSIXct(NA))

  if (nrow(prior) > 0) {
    vanished <- prior[!(prior$survey_id %in% discovered$survey_id), , drop = FALSE]
    if (nrow(vanished) > 0) {
      freshly_vanished <- is.na(vanished$is_deleted) | !vanished$is_deleted
      vanished$is_deleted[freshly_vanished] <- TRUE
      vanished$deleted_detected_at[freshly_vanished] <- now
      discovered <- dplyr::bind_rows(discovered, vanished[names(discovered)])
    }
  }

  DBI::dbExecute(con, glue::glue("DELETE FROM {ducklake_alias}.raw.surveys"))
  write_rows(con, "surveys", discovered)
  invisible(discovered)
}

# The only mechanism that can detect a deletion is a full refresh (ADR-005):
# the new row set is (fetched) UNION (previously stored but absent from the
# fetch, carried forward with is_deleted = TRUE). A response once flagged
# stays flagged unless the API returns it again.
carry_forward_deleted <- function(con, survey_id, fetched, now = Sys.time()) {
  fetched <- dplyr::mutate(fetched, is_deleted = FALSE, deleted_detected_at = as.POSIXct(NA))
  prior <- DBI::dbGetQuery(con, glue::glue(
    "SELECT * FROM {ducklake_alias}.raw.responses WHERE survey_id = {DBI::dbQuoteString(con, survey_id)}"
  ))
  if (nrow(prior) == 0) {
    return(fetched)
  }
  vanished <- prior[!(prior$response_id %in% fetched$response_id), , drop = FALSE]
  if (nrow(vanished) == 0) {
    return(fetched)
  }
  freshly_vanished <- is.na(vanished$is_deleted) | !vanished$is_deleted
  vanished$is_deleted[freshly_vanished] <- TRUE
  vanished$deleted_detected_at[freshly_vanished] <- now
  dplyr::bind_rows(fetched, vanished[names(fetched)])
}

# Fetches everything for one survey and rebuilds its rows in every raw table
# inside a single transaction (one DuckLake snapshot per survey per run).
# Integrity is asserted before the transaction commits (ADR-006): a failure
# rolls the survey back, leaving its prior state exactly as it was.
refresh_survey <- function(con, client, survey_id, run_id, include, modified_on, probe) {
  now <- Sys.time()
  stamp <- function(df) if (nrow(df) == 0) df else dplyr::mutate(df, ingested_at = now, run_id = run_id)
  n_fetched <- NA_integer_

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

      campaigns <- if ("campaigns" %in% include) {
        parse_campaigns(survey_id, alchemer_fetch_all(client, glue::glue("survey/{survey_id}/surveycampaign")))
      } else {
        tibble::tibble()
      }
      statistics <- if ("statistics" %in% include) {
        parse_statistics(survey_id, alchemer_fetch_all(client, glue::glue("survey/{survey_id}/surveystatistic")))
      } else {
        tibble::tibble()
      }

      fetched_responses <- parse_responses(survey_id, alchemer_fetch_all(client, glue::glue("survey/{survey_id}/surveyresponse")))
      n_fetched <- nrow(fetched_responses)

      responses <- stamp(carry_forward_deleted(con, survey_id, fetched_responses, now))
      replace_survey_rows(con, "survey_definitions", survey_id, stamp(parsed_def$definition))
      replace_survey_rows(con, "survey_pages", survey_id, stamp(parsed_def$pages))
      replace_survey_rows(con, "survey_questions", survey_id, stamp(parsed_def$questions))
      replace_survey_rows(con, "survey_question_options", survey_id, stamp(parsed_def$options))
      replace_survey_rows(con, "survey_campaigns", survey_id, stamp(campaigns))
      replace_survey_rows(con, "survey_statistics", survey_id, stamp(statistics))
      replace_survey_rows(con, "responses", survey_id, responses)

      checks <- run_integrity_checks(con, survey_id = survey_id, expected_count = n_fetched)
      failed <- checks[!checks$passed, , drop = FALSE]
      if (nrow(failed) > 0) {
        stop(paste("integrity check(s) failed:", paste(failed$message, collapse = "; ")), call. = FALSE)
      }

      update_survey_state(con, survey_id, modified_on, probe, success = TRUE, now = now)
      write_rows_generic(con, "meta.integrity_checks", dplyr::mutate(checks, run_id = run_id, checked_at = now))
      list(status = "ok", message = "OK")
    },
    error = function(e) list(status = "error", message = conditionMessage(e))
  )

  if (identical(result$status, "ok")) {
    DBI::dbExecute(con, "COMMIT")
  } else {
    DBI::dbExecute(con, "ROLLBACK")
    update_survey_state(con, survey_id, modified_on, probe, success = FALSE, now = now)
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
#' @param full_sweep_days Maximum days since a survey's last successful
#'   refresh before it is refreshed regardless of change detection --
#'   a correctness backstop, not a freshness setting (ADR-004).
#' @param rpm Request-per-minute throttle ceiling, passed to [alchemer_client()]
#'   when `client` is not supplied directly.
#' @param dry_run If `TRUE`, make the same decisions and requests but write
#'   nothing to the database.
#' @param client An `alchemer_client`. Defaults to one built from the
#'   environment; tests pass a fixture-backed client (ADR-010).
#' @return Invisibly, a tibble with one row per survey considered: decision,
#'   counts, timings, and status.
#' @export
ingest <- function(db = alchemer_db_path(), surveys = NULL, force = FALSE,
                    include = c("campaigns", "statistics"),
                    full_sweep_days = alchemer_full_sweep_days(), rpm = alchemer_rpm(),
                    dry_run = FALSE, client = alchemer_client(rpm = rpm)) {
  run_id <- new_run_id()
  run_started_at <- Sys.time()
  con <- alchemer_db(db)
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)

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

    probe <- tryCatch(probe_survey(client, survey_id), error = function(e) NULL)

    decision <- if (!is.null(surveys)) {
      list(refresh = TRUE, reason = "explicit surveys= argument")
    } else if (is.null(probe)) {
      list(refresh = TRUE, reason = "probe request failed; refreshing to be safe")
    } else {
      decide_refresh(prior_state, modified_on, probe, force, full_sweep_days)
    }

    if (!dry_run) {
      log_event(con, run_id, survey_id, "decision", "info", message = decision$reason, started_at = t0, finished_at = Sys.time())
    }

    if (!decision$refresh) {
      return(tibble::tibble(
        survey_id = survey_id, decision = decision$reason, refreshed = FALSE,
        n_fetched = NA_integer_, status = "skipped", started_at = t0, finished_at = Sys.time()
      ))
    }
    if (dry_run) {
      return(tibble::tibble(
        survey_id = survey_id, decision = decision$reason, refreshed = TRUE,
        n_fetched = NA_integer_, status = "dry_run", started_at = t0, finished_at = Sys.time()
      ))
    }

    outcome <- tryCatch(
      refresh_survey(con, client, survey_id, run_id, include, modified_on, probe %||% list(total_count = NA_integer_, max_date_updated = NA_character_)),
      error = function(e) list(status = "error", message = conditionMessage(e), n_fetched = NA_integer_)
    )
    log_event(
      con, run_id, survey_id, "refresh", outcome$status, message = outcome$message,
      started_at = t0, finished_at = Sys.time(), n_responses = outcome$n_fetched
    )

    tibble::tibble(
      survey_id = survey_id, decision = decision$reason, refreshed = TRUE,
      n_fetched = outcome$n_fetched, status = outcome$status, started_at = t0, finished_at = Sys.time()
    )
  })

  out <- if (length(results) == 0) tibble::tibble() else dplyr::bind_rows(results)

  if (!dry_run) {
    write_rows_generic(con, "meta.run_events", tibble::tibble(
      run_id = run_id, survey_id = NA_character_, phase = "run", status = "finished",
      http_status = NA_integer_, message = NA_character_,
      started_at = run_started_at, finished_at = Sys.time(), n_responses = NA_integer_
    ))
    DBI::dbExecute(con, glue::glue("DELETE FROM {ducklake_alias}.meta.runs WHERE run_id = {DBI::dbQuoteString(con, run_id)}"))
    write_rows_generic(con, "meta.runs", tibble::tibble(
      run_id = run_id, started_at = run_started_at, finished_at = Sys.time(),
      status = if (nrow(out) > 0 && any(out$status == "error")) "completed_with_errors" else "completed",
      package_version = as.character(utils::packageVersion("alchemeR")),
      duckdb_version = as.character(utils::packageVersion("duckdb")),
      n_checked = nrow(out), n_refreshed = sum(out$refreshed %in% TRUE),
      n_failed = sum(out$status == "error"), n_requests = NA_integer_
    ))
  }

  invisible(out)
}
