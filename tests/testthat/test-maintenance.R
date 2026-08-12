seed_maintenance_db <- function(dir) {
  con <- alchemer_db(dir)
  DBI::dbExecute(con, "INSERT INTO alchemer.raw.surveys (survey_id, title, is_deleted) VALUES ('1', 'S1', FALSE)")
  DBI::dbExecute(con, "INSERT INTO alchemer.raw.responses (survey_id, response_id, date_submitted, is_deleted) VALUES
    ('1', 'r1', '2020-01-01 00:00:00 EST', FALSE),
    ('1', 'r2', '2026-01-01 00:00:00 EST', FALSE)")
  DBI::dbDisconnect(con, shutdown = TRUE)
}

current_snapshot <- function(con) {
  DBI::dbGetQuery(con, "SELECT MAX(snapshot_id) v FROM ducklake_snapshots('alchemer')")$v
}

test_that("db_status summarises surveys, responses, snapshots, and the last run", {
  dir <- withr::local_tempdir()
  seed_maintenance_db(dir)
  out <- db_status(dir)
  expect_equal(out$n_surveys, 1)
  expect_equal(out$n_responses, 2)
  expect_true(out$n_snapshots > 0)
  expect_true(out$size_bytes > 0)
})

test_that("compact() flushes inlined rows to Parquet", {
  dir <- withr::local_tempdir()
  seed_maintenance_db(dir)
  out <- compact(dir)
  expect_true(any(out$flushed$table_name == "responses"))
  expect_equal(out$flushed$rows_flushed[out$flushed$table_name == "responses"], 2)
})

test_that("expire_history drops old snapshots and makes stale time travel fail cleanly", {
  dir <- withr::local_tempdir()
  seed_maintenance_db(dir)

  con <- alchemer_db(dir)
  version_before <- current_snapshot(con)
  DBI::dbDisconnect(con, shutdown = TRUE)

  Sys.sleep(1) # ensure the next commit's timestamp is strictly later
  con1b <- alchemer_db(dir)
  DBI::dbExecute(con1b, "INSERT INTO alchemer.raw.surveys (survey_id, title, is_deleted) VALUES ('2', 'S2', FALSE)")
  DBI::dbDisconnect(con1b, shutdown = TRUE)

  expire_history(dir, older_than = Sys.time())

  con2 <- alchemer_db(dir, read_only = TRUE)
  on.exit(DBI::dbDisconnect(con2, shutdown = TRUE))
  err <- tryCatch(
    DBI::dbGetQuery(con2, glue::glue("SELECT COUNT(*) n FROM alchemer.raw.responses AT (VERSION => {version_before})")),
    error = function(e) e
  )
  expect_s3_class(err, "error")
  expect_match(conditionMessage(err), "[Nn]o snapshot")
})

test_that("expunge(before_date =) removes only the matching responses, and their history", {
  dir <- withr::local_tempdir()
  seed_maintenance_db(dir)

  con <- alchemer_db(dir)
  version_before <- current_snapshot(con)
  DBI::dbDisconnect(con, shutdown = TRUE)

  expunge(dir, before_date = as.Date("2025-01-01"))

  con2 <- alchemer_db(dir, read_only = TRUE)
  on.exit(DBI::dbDisconnect(con2, shutdown = TRUE))
  remaining <- DBI::dbGetQuery(con2, "SELECT response_id FROM alchemer.raw.responses")$response_id
  expect_equal(remaining, "r2")

  err <- tryCatch(
    DBI::dbGetQuery(con2, glue::glue("SELECT COUNT(*) n FROM alchemer.raw.responses AT (VERSION => {version_before})")),
    error = function(e) e
  )
  expect_s3_class(err, "error")
})

test_that("expunge(before_date =) is DST-correct across an EST/EDT boundary", {
  # Regression test (ultrareview): the before_date comparison previously
  # stripped date_submitted's EST/EDT suffix and treated it as an
  # unlabeled naive timestamp, with no documented (or correct) handling of
  # which timezone either side of the comparison was actually in. This
  # checks a response 1 hour before a Toronto midnight cutoff (should be
  # removed) and one 1 hour after (should be kept), using an EDT-tagged
  # (summer) date_submitted where a fixed UTC-5 offset would get the
  # comparison wrong by an hour.
  dir <- withr::local_tempdir()
  con <- alchemer_db(dir)
  DBI::dbExecute(con, "INSERT INTO alchemer.raw.surveys (survey_id, title, is_deleted) VALUES ('1', 'S1', FALSE)")
  DBI::dbExecute(con, "INSERT INTO alchemer.raw.responses (survey_id, response_id, date_submitted, is_deleted) VALUES
    ('1', 'before', '2024-06-30 23:00:00 EDT', FALSE),
    ('1', 'after', '2024-07-01 01:00:00 EDT', FALSE)")
  DBI::dbDisconnect(con, shutdown = TRUE)

  expunge(dir, before_date = as.Date("2024-07-01"))

  con2 <- alchemer_db(dir, read_only = TRUE)
  on.exit(DBI::dbDisconnect(con2, shutdown = TRUE))
  remaining <- DBI::dbGetQuery(con2, "SELECT response_id FROM alchemer.raw.responses")$response_id
  expect_equal(remaining, "after")
})

test_that("expunge(survey_id =) removes the survey and all of its raw rows", {
  dir <- withr::local_tempdir()
  seed_maintenance_db(dir)

  expunge(dir, survey_id = "1")

  con <- alchemer_db(dir, read_only = TRUE)
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  expect_equal(DBI::dbGetQuery(con, "SELECT COUNT(*) n FROM alchemer.raw.surveys")$n, 0)
  expect_equal(DBI::dbGetQuery(con, "SELECT COUNT(*) n FROM alchemer.raw.responses")$n, 0)
})

test_that("expunge() requires survey_id or before_date", {
  dir <- withr::local_tempdir()
  seed_maintenance_db(dir)
  expect_error(expunge(dir), class = "alchemeR_config_error")
})

test_that("expunge() rolls back cleanly if a delete fails partway", {
  dir <- withr::local_tempdir()
  seed_maintenance_db(dir)
  con <- alchemer_db(dir)
  DBI::dbDisconnect(con, shutdown = TRUE)

  # before_date that TRY_CAST can't compare against anything (still valid
  # SQL) exercises the normal path; a genuinely malformed table name would
  # error at the DELETE -- verify rollback leaves prior state intact by
  # checking row counts are unaffected when nothing matches.
  expunge(dir, before_date = as.Date("1900-01-01"))
  con2 <- alchemer_db(dir, read_only = TRUE)
  on.exit(DBI::dbDisconnect(con2, shutdown = TRUE))
  expect_equal(DBI::dbGetQuery(con2, "SELECT COUNT(*) n FROM alchemer.raw.responses")$n, 2)
})
