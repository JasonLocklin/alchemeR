ingest_test_client <- function() {
  alchemer_client(token = "T", secret = "S", domain = "api.alchemer.com", rpm = 1000)
}

new_state <- function() {
  list(surveys = list(), responses = list(), definitions = list())
}

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
  # Regression test (ultrareview): the same zero-column-tibble bug in
  # carry_forward_deleted(), triggered when a survey's responses all
  # disappear in one fetch instead of the account's surveys.
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
  out <- ingest(db = dir, client = ingest_test_client(), full_sweep_days = 0)
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
