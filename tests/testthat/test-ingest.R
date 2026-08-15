ingest_test_client <- function() {
  new_alchemer_client("T", "S", domain = "api.alchemer.com", rpm = 1000)
}

new_state <- function() {
  list(surveys = list(), responses = list(), definitions = list())
}

test_that("probe_survey handles a zero-response survey without crashing", {
  # Regression test (ultrareview): body$data[[1]] on an empty `data` array
  # (a survey with zero responses) threw "subscript out of bounds", masked
  # by ingest()'s outer tryCatch but permanently defeating change detection
  # for that survey (it always fell back to "probe failed; refresh anyway").
  httr2::local_mocked_responses(list(httr2::response_json(status_code = 200, body = list(
    result_ok = TRUE, total_count = 0, page = 1, total_pages = 1, data = list()
  ))))
  out <- probe_survey(ingest_test_client(), "1")
  expect_equal(out$total_count, 0L)
  expect_true(is.na(out$max_date_updated))
})

test_that("decide_refresh: never-refreshed surveys always refresh", {
  out <- decide_refresh(NULL, "2026-01-01", list(total_count = 0, max_date_updated = NA), FALSE, 90)
  expect_true(out$refresh)
  expect_match(out$reason, "never")
})

test_that("decide_refresh: unchanged modified_on/probe skips", {
  prior <- data.frame(
    last_modified_on = "2026-01-01", last_probe_total_count = 5L,
    last_probe_max_date_updated = "2026-01-01 00:00:00",
    last_successful_refresh_at = Sys.time() - 60
  )
  probe <- list(total_count = 5L, max_date_updated = "2026-01-01 00:00:00")
  out <- decide_refresh(prior, "2026-01-01", probe, FALSE, 90)
  expect_false(out$refresh)
})

test_that("decide_refresh: a changed probe triggers a refresh, never a filter", {
  prior <- data.frame(
    last_modified_on = "2026-01-01", last_probe_total_count = 5L,
    last_probe_max_date_updated = "2026-01-01 00:00:00",
    last_successful_refresh_at = Sys.time() - 60
  )
  probe <- list(total_count = 6L, max_date_updated = "2026-01-01 00:00:00")
  out <- decide_refresh(prior, "2026-01-01", probe, FALSE, 90)
  expect_true(out$refresh)
  expect_match(out$reason, "probe")
})

test_that("decide_refresh: full_sweep_days = 0 forces a refresh even with no other change", {
  prior <- data.frame(
    last_modified_on = "2026-01-01", last_probe_total_count = 5L,
    last_probe_max_date_updated = "2026-01-01 00:00:00",
    last_successful_refresh_at = Sys.time() - 1
  )
  probe <- list(total_count = 5L, max_date_updated = "2026-01-01 00:00:00")
  out <- decide_refresh(prior, "2026-01-01", probe, FALSE, 0)
  expect_true(out$refresh)
  expect_match(out$reason, "full_sweep_days")
})

test_that("cold run populates surveys, definitions, questions, and responses", {
  dir <- withr::local_tempdir()
  state <- new_state()
  state$surveys <- list(mock_survey("1"))
  state$responses <- list("1" = list(mock_response("r1"), mock_response("r2")))

  httr2::local_mocked_responses(mock_client_router(state))
  out <- ingest(db = dir, client = ingest_test_client())

  expect_equal(nrow(out), 1)
  expect_equal(out$status, "ok")
  expect_equal(out$n_fetched, 2)

  con <- alchemer_db(dir, read_only = TRUE)
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  expect_equal(DBI::dbGetQuery(con, "SELECT COUNT(*) n FROM alchemer.raw.surveys")$n, 1)
  expect_equal(DBI::dbGetQuery(con, "SELECT COUNT(*) n FROM alchemer.raw.responses")$n, 2)
  expect_equal(DBI::dbGetQuery(con, "SELECT COUNT(*) n FROM alchemer.meta.survey_state")$n, 1)
  expect_equal(DBI::dbGetQuery(con, "SELECT COUNT(*) n FROM alchemer.meta.runs")$n, 1)
})

test_that("a zero-response survey's change detection works instead of always force-refreshing", {
  # Regression test (ultrareview): before the probe_survey() fix above, a
  # zero-response survey's probe always threw internally, was caught by
  # ingest()'s outer tryCatch, and fell back to "refresh anyway" on every
  # run -- change detection never actually engaged for it.
  dir <- withr::local_tempdir()
  state <- new_state()
  state$surveys <- list(mock_survey("1"))
  state$responses <- list("1" = list())

  httr2::local_mocked_responses(mock_client_router(state))
  ingest(db = dir, client = ingest_test_client())

  httr2::local_mocked_responses(mock_client_router(state))
  out2 <- ingest(db = dir, client = ingest_test_client())

  expect_equal(out2$status, "skipped")
  expect_match(out2$decision, "no change")
})

test_that("an immediate second run refreshes nothing (idempotence)", {
  dir <- withr::local_tempdir()
  state <- new_state()
  state$surveys <- list(mock_survey("1"))
  state$responses <- list("1" = list(mock_response("r1")))

  httr2::local_mocked_responses(mock_client_router(state))
  ingest(db = dir, client = ingest_test_client())

  httr2::local_mocked_responses(mock_client_router(state))
  out2 <- ingest(db = dir, client = ingest_test_client())

  expect_equal(out2$status, "skipped")
  expect_match(out2$decision, "no change")
})

test_that("a changed probe triggers exactly one of several surveys", {
  dir <- withr::local_tempdir()
  state <- new_state()
  state$surveys <- list(mock_survey("1"), mock_survey("2"))
  state$responses <- list("1" = list(mock_response("r1")), "2" = list(mock_response("r2")))

  httr2::local_mocked_responses(mock_client_router(state))
  ingest(db = dir, client = ingest_test_client())

  state$responses[["2"]] <- list(mock_response("r2"), mock_response("r3", date_updated = "2026-02-01 00:00:00"))
  httr2::local_mocked_responses(mock_client_router(state))
  out <- ingest(db = dir, client = ingest_test_client())

  expect_equal(out$status[out$survey_id == "1"], "skipped")
  expect_equal(out$status[out$survey_id == "2"], "ok")
  expect_equal(out$n_fetched[out$survey_id == "2"], 2)
})

test_that("a removed response is flagged, not deleted", {
  dir <- withr::local_tempdir()
  state <- new_state()
  state$surveys <- list(mock_survey("1"))
  state$responses <- list("1" = list(mock_response("r1"), mock_response("r2")))

  httr2::local_mocked_responses(mock_client_router(state))
  ingest(db = dir, client = ingest_test_client())

  state$responses[["1"]] <- list(mock_response("r1")) # r2 vanishes
  httr2::local_mocked_responses(mock_client_router(state))
  ingest(db = dir, client = ingest_test_client(), force = TRUE)

  con <- alchemer_db(dir, read_only = TRUE)
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  rows <- DBI::dbGetQuery(con, "SELECT response_id, is_deleted FROM alchemer.raw.responses ORDER BY response_id")
  expect_equal(rows$response_id, c("r1", "r2"))
  expect_false(rows$is_deleted[rows$response_id == "r1"])
  expect_true(rows$is_deleted[rows$response_id == "r2"])
})

test_that("a removed survey is flagged, not dropped", {
  dir <- withr::local_tempdir()
  state <- new_state()
  state$surveys <- list(mock_survey("1"), mock_survey("2"))
  state$responses <- list("1" = list(mock_response("r1")), "2" = list(mock_response("r2")))

  httr2::local_mocked_responses(mock_client_router(state))
  ingest(db = dir, client = ingest_test_client())

  state$surveys <- list(mock_survey("1")) # survey 2 vanishes from discovery
  httr2::local_mocked_responses(mock_client_router(state))
  ingest(db = dir, client = ingest_test_client())

  con <- alchemer_db(dir, read_only = TRUE)
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  rows <- DBI::dbGetQuery(con, "SELECT survey_id, is_deleted FROM alchemer.raw.surveys ORDER BY survey_id")
  expect_equal(rows$survey_id, c("1", "2"))
  expect_false(rows$is_deleted[rows$survey_id == "1"])
  expect_true(rows$is_deleted[rows$survey_id == "2"])
})

test_that("every survey vanishing from discovery at once flags them all, without crashing", {
  # Regression test (ultrareview): discovery returning zero surveys produced
  # a zero-column tibble, which upsert_surveys() combined with previously-
  # stored rows and then subset by column name -- losing every real column
  # and crashing the whole run with no meta.runs record.
  dir <- withr::local_tempdir()
  state <- new_state()
  state$surveys <- list(mock_survey("1"), mock_survey("2"))
  state$responses <- list("1" = list(mock_response("r1")), "2" = list(mock_response("r2")))

  httr2::local_mocked_responses(mock_client_router(state))
  ingest(db = dir, client = ingest_test_client())

  state$surveys <- list() # the whole account appears to have zero surveys
  httr2::local_mocked_responses(mock_client_router(state))
  out <- ingest(db = dir, client = ingest_test_client())

  con <- alchemer_db(dir, read_only = TRUE)
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  rows <- DBI::dbGetQuery(con, "SELECT survey_id, is_deleted FROM alchemer.raw.surveys ORDER BY survey_id")
  expect_equal(rows$survey_id, c("1", "2"))
  expect_true(all(rows$is_deleted))
  expect_equal(DBI::dbGetQuery(con, "SELECT COUNT(*) n FROM alchemer.meta.runs")$n, 2)
})

test_that("every response vanishing from a survey at once flags them all, without crashing", {
  # Regression test (ultrareview): the same zero-column-tibble bug, triggered
  # when a survey's responses all disappear in one fetch instead of the
  # account's surveys. "Nothing fetched" has to mean "flag every stored row",
  # not "crash".
  dir <- withr::local_tempdir()
  state <- new_state()
  state$surveys <- list(mock_survey("1"))
  state$responses <- list("1" = list(mock_response("r1"), mock_response("r2")))

  httr2::local_mocked_responses(mock_client_router(state))
  ingest(db = dir, client = ingest_test_client())

  state$responses[["1"]] <- list() # the survey now reports zero responses
  httr2::local_mocked_responses(mock_client_router(state))
  out <- ingest(db = dir, client = ingest_test_client(), force = TRUE)

  expect_equal(out$status, "ok")
  con <- alchemer_db(dir, read_only = TRUE)
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  rows <- DBI::dbGetQuery(con, "SELECT response_id, is_deleted FROM alchemer.raw.responses ORDER BY response_id")
  expect_equal(rows$response_id, c("r1", "r2"))
  expect_true(all(rows$is_deleted))
})

test_that("an error mid-survey leaves prior state intact and other surveys committed", {
  dir <- withr::local_tempdir()
  state <- new_state()
  state$surveys <- list(mock_survey("1"), mock_survey("2"))
  state$responses <- list("1" = list(mock_response("r1")), "2" = list(mock_response("r2")))

  httr2::local_mocked_responses(mock_client_router(state))
  ingest(db = dir, client = ingest_test_client())

  # Force a definition-tree fetch failure for survey 1 only, by having its
  # GET /survey/1 return result_ok: false; survey 2's data also changes so
  # both are refresh candidates.
  broken_router <- function(req) {
    parsed <- httr2::url_parse(req$url)
    if (grepl("/survey/1$", parsed$path)) {
      return(httr2::response_json(status_code = 200, body = list(result_ok = FALSE, code = 500, message = "boom")))
    }
    mock_client_router(state)(req)
  }
  state$responses[["2"]] <- list(mock_response("r2"), mock_response("r3", date_updated = "2026-03-01 00:00:00"))
  httr2::local_mocked_responses(broken_router)
  out <- ingest(db = dir, client = ingest_test_client(), force = TRUE)

  expect_equal(out$status[out$survey_id == "1"], "error")
  expect_equal(out$status[out$survey_id == "2"], "ok")

  con <- alchemer_db(dir, read_only = TRUE)
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  # survey 1's responses are untouched by the failed refresh
  expect_equal(DBI::dbGetQuery(con, "SELECT COUNT(*) n FROM alchemer.raw.responses WHERE survey_id = '1'")$n, 1)
  # survey 2's successful refresh is fully committed
  expect_equal(DBI::dbGetQuery(con, "SELECT COUNT(*) n FROM alchemer.raw.responses WHERE survey_id = '2'")$n, 2)
  failures <- DBI::dbGetQuery(con, "SELECT consecutive_failures FROM alchemer.meta.survey_state WHERE survey_id = '1'")
  expect_equal(failures$consecutive_failures, 1L)
})

test_that("full_sweep_days = 0 forces every survey to refresh", {
  dir <- withr::local_tempdir()
  state <- new_state()
  state$surveys <- list(mock_survey("1"))
  state$responses <- list("1" = list(mock_response("r1")))

  httr2::local_mocked_responses(mock_client_router(state))
  ingest(db = dir, client = ingest_test_client())

  httr2::local_mocked_responses(mock_client_router(state))
  withr::local_envvar(ALCHEMER_FULL_SWEEP_DAYS = "0")
  out <- ingest(db = dir, client = ingest_test_client())
  expect_equal(out$status, "ok")
  expect_match(out$decision, "full_sweep_days")
})

test_that("dry_run reports decisions without writing anything", {
  dir <- withr::local_tempdir()
  state <- new_state()
  state$surveys <- list(mock_survey("1"))
  state$responses <- list("1" = list(mock_response("r1")))

  httr2::local_mocked_responses(mock_client_router(state))
  out <- ingest(db = dir, client = ingest_test_client(), dry_run = TRUE)
  expect_equal(out$status, "dry_run")

  con <- alchemer_db(dir, read_only = TRUE)
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  expect_equal(DBI::dbGetQuery(con, "SELECT COUNT(*) n FROM alchemer.raw.surveys")$n, 0)
  expect_equal(DBI::dbGetQuery(con, "SELECT COUNT(*) n FROM alchemer.meta.runs")$n, 0)
})

test_that("re-refreshing unchanged data rewrites nothing", {
  # The property the merge exists for. DuckLake never rewrites a Parquet file
  # in place, so deleting and re-inserting a whole survey wrote a fresh copy of
  # every row on every run -- measured at 2.1 MB -> 26.7 MB over 12 refreshes of
  # a 5,000-response survey, all retained until snapshots expire.
  #
  # `ingested_at` is the observable proxy: it is only stamped when a row is
  # actually written, so an unchanged row keeping its original value is proof
  # that nothing was rewritten. A regression to replace-wholesale would give
  # every row a fresh timestamp and fail here.
  dir <- withr::local_tempdir()
  state <- new_state()
  state$surveys <- list(mock_survey("1"))
  state$responses <- list("1" = list(mock_response("r1"), mock_response("r2")))

  httr2::local_mocked_responses(mock_client_router(state))
  ingest(db = dir, client = ingest_test_client())

  con <- alchemer_db(dir, read_only = TRUE)
  before <- DBI::dbGetQuery(con, "SELECT response_id, ingested_at FROM alchemer.raw.responses
    ORDER BY response_id")
  DBI::dbDisconnect(con, shutdown = TRUE)

  Sys.sleep(0.01) # so a rewrite would be visibly newer
  httr2::local_mocked_responses(mock_client_router(state))
  ingest(db = dir, client = ingest_test_client(), force = TRUE)

  con <- alchemer_db(dir, read_only = TRUE)
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  after <- DBI::dbGetQuery(con, "SELECT response_id, ingested_at FROM alchemer.raw.responses
    ORDER BY response_id")
  expect_equal(after$response_id, before$response_id)
  expect_equal(after$ingested_at, before$ingested_at)
})

test_that("a changed response is rewritten, an unchanged one beside it is not", {
  dir <- withr::local_tempdir()
  state <- new_state()
  state$surveys <- list(mock_survey("1"))
  state$responses <- list("1" = list(mock_response("r1"), mock_response("r2")))

  httr2::local_mocked_responses(mock_client_router(state))
  ingest(db = dir, client = ingest_test_client())

  con <- alchemer_db(dir, read_only = TRUE)
  before <- DBI::dbGetQuery(con, "SELECT response_id, ingested_at FROM alchemer.raw.responses
    ORDER BY response_id")
  DBI::dbDisconnect(con, shutdown = TRUE)

  Sys.sleep(0.01)
  edited <- mock_response("r2", date_updated = "2026-05-01 00:00:00")
  edited$status <- "Partial"
  state$responses[["1"]] <- list(mock_response("r1"), edited)
  httr2::local_mocked_responses(mock_client_router(state))
  ingest(db = dir, client = ingest_test_client())

  con <- alchemer_db(dir, read_only = TRUE)
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  after <- DBI::dbGetQuery(con, "SELECT response_id, status, ingested_at FROM alchemer.raw.responses
    ORDER BY response_id")
  expect_equal(after$status[after$response_id == "r2"], "Partial")
  expect_gt(after$ingested_at[after$response_id == "r2"], before$ingested_at[before$response_id == "r2"])
  expect_equal(after$ingested_at[after$response_id == "r1"], before$ingested_at[before$response_id == "r1"])
})

test_that("a response that reappears upstream is un-flagged", {
  # is_deleted is one of the compared columns, so a returning row differs from
  # the flagged stored one and the merge updates it. Without that, a response
  # deleted and then restored in Alchemer would stay flagged forever.
  dir <- withr::local_tempdir()
  state <- new_state()
  state$surveys <- list(mock_survey("1"))
  state$responses <- list("1" = list(mock_response("r1"), mock_response("r2")))

  httr2::local_mocked_responses(mock_client_router(state))
  ingest(db = dir, client = ingest_test_client())

  state$responses[["1"]] <- list(mock_response("r1")) # r2 vanishes
  httr2::local_mocked_responses(mock_client_router(state))
  ingest(db = dir, client = ingest_test_client(), force = TRUE)

  con <- alchemer_db(dir, read_only = TRUE)
  flagged <- DBI::dbGetQuery(con, "SELECT response_id FROM alchemer.raw.responses WHERE is_deleted")
  DBI::dbDisconnect(con, shutdown = TRUE)
  expect_equal(flagged$response_id, "r2")

  state$responses[["1"]] <- list(mock_response("r1"), mock_response("r2")) # ...and returns
  httr2::local_mocked_responses(mock_client_router(state))
  ingest(db = dir, client = ingest_test_client(), force = TRUE)

  con <- alchemer_db(dir, read_only = TRUE)
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  rows <- DBI::dbGetQuery(con, "SELECT response_id, is_deleted, deleted_detected_at
    FROM alchemer.raw.responses ORDER BY response_id")
  expect_equal(rows$response_id, c("r1", "r2"))
  expect_false(any(rows$is_deleted))
  expect_true(all(is.na(rows$deleted_detected_at)))
})

test_that("omitting a dataset from include= leaves what was archived alone", {
  # Regression test (code review): an excluded dataset used to be passed an
  # empty frame, and an empty fetch means "every stored row vanished upstream",
  # so its previously archived rows were destroyed. Omitting a dataset means
  # "don't fetch it this run", never "delete it".
  dir <- withr::local_tempdir()
  state <- new_state()
  state$surveys <- list(mock_survey("1"))
  state$responses <- list("1" = list(mock_response("r1")))
  router <- function(req) {
    path <- httr2::url_parse(req$url)$path
    if (grepl("/surveycampaign$", path)) {
      return(httr2::response_json(status_code = 200, body = list(
        result_ok = TRUE, total_count = 1, page = 1, total_pages = 1,
        data = list(list(id = "9001", name = "Default Link"))
      )))
    }
    mock_client_router(state)(req)
  }

  httr2::local_mocked_responses(router)
  ingest(db = dir, client = ingest_test_client(), include = "campaigns")

  con <- alchemer_db(dir, read_only = TRUE)
  expect_equal(DBI::dbGetQuery(con, "SELECT COUNT(*) n FROM alchemer.raw.survey_campaigns")$n, 1)
  DBI::dbDisconnect(con, shutdown = TRUE)

  httr2::local_mocked_responses(router)
  ingest(db = dir, client = ingest_test_client(), include = character(0), force = TRUE)

  con <- alchemer_db(dir, read_only = TRUE)
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  expect_equal(DBI::dbGetQuery(con, "SELECT COUNT(*) n FROM alchemer.raw.survey_campaigns")$n, 1)
})

test_that("a failed integrity check is recorded, not discarded with the rollback", {
  # Regression test (code review): the checks were written inside the refresh
  # transaction and only after the pass/fail branch, so meta.integrity_checks
  # could contain nothing but passes -- inverting its meaning for monitoring,
  # since "no failed rows" did not mean "no failed checks".
  # The failure has to come from the API, not from a row injected beforehand: a
  # refresh replaces everything it stores for the survey, so anything injected
  # into raw is gone before the checks run. A list endpoint returning the same
  # response twice -- what an overlapping page would look like -- is the real
  # shape of this, and the assertion exists precisely because DuckLake cannot
  # declare the uniqueness constraint that would otherwise catch it.
  dir <- withr::local_tempdir()
  state <- new_state()
  state$surveys <- list(mock_survey("1"))
  state$responses <- list("1" = list(mock_response("r1"), mock_response("r1")))

  httr2::local_mocked_responses(mock_client_router(state))
  out <- ingest(db = dir, client = ingest_test_client())
  expect_equal(out$status, "error")

  con <- alchemer_db(dir, read_only = TRUE)
  # The rollback held: no half-written survey.
  expect_equal(DBI::dbGetQuery(con, "SELECT COUNT(*) n FROM alchemer.raw.responses")$n, 0)
  DBI::dbDisconnect(con, shutdown = TRUE)

  con <- alchemer_db(dir, read_only = TRUE)
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  failed <- DBI::dbGetQuery(con, "SELECT check_name, message FROM alchemer.meta.integrity_checks
    WHERE NOT passed")
  expect_gt(nrow(failed), 0)
  expect_true(any(grepl("duplicate", failed$message)))
})

test_that("meta.runs records how many API requests the run actually made", {
  dir <- withr::local_tempdir()
  state <- new_state()
  state$surveys <- list(mock_survey("1"), mock_survey("2"))
  state$responses <- list("1" = list(mock_response("r1")), "2" = list(mock_response("r2")))

  httr2::local_mocked_responses(mock_client_router(state))
  ingest(db = dir, client = ingest_test_client())

  con <- alchemer_db(dir, read_only = TRUE)
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  n_requests <- DBI::dbGetQuery(con, "SELECT n_requests FROM alchemer.meta.runs")$n_requests
  # 1 discovery + 2 probes + 5 per refreshed survey (definition, questions,
  # campaigns, statistics, responses) = 13. Asserted as a bound rather than an
  # exact figure so adding a fetch doesn't fail the test for no reason, but a
  # per-survey request explosion would.
  expect_gt(n_requests, 0)
  expect_lte(n_requests, 1 + 2 * (1 + 5))
})

test_that("time travel returns the pre-refresh state", {
  dir <- withr::local_tempdir()
  state <- new_state()
  state$surveys <- list(mock_survey("1"))
  state$responses <- list("1" = list(mock_response("r1")))

  httr2::local_mocked_responses(mock_client_router(state))
  ingest(db = dir, client = ingest_test_client())

  con <- alchemer_db(dir, read_only = TRUE)
  version_before <- DBI::dbGetQuery(con, "SELECT MAX(snapshot_id) v FROM ducklake_snapshots('alchemer')")$v
  DBI::dbDisconnect(con, shutdown = TRUE)

  state$responses[["1"]] <- list(mock_response("r1"), mock_response("r2", date_updated = "2026-04-01 00:00:00"))
  httr2::local_mocked_responses(mock_client_router(state))
  ingest(db = dir, client = ingest_test_client(), force = TRUE)

  con2 <- alchemer_db(dir, read_only = TRUE)
  on.exit(DBI::dbDisconnect(con2, shutdown = TRUE))
  now_count <- DBI::dbGetQuery(con2, "SELECT COUNT(*) n FROM alchemer.raw.responses")$n
  past_count <- DBI::dbGetQuery(con2, glue::glue(
    "SELECT COUNT(*) n FROM alchemer.raw.responses AT (VERSION => {version_before})"
  ))$n
  expect_equal(now_count, 2)
  expect_equal(past_count, 1)
})
