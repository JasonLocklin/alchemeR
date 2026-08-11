test_that("alchemer_db creates a fresh DuckLake catalog with every schema", {
  dir <- withr::local_tempdir()
  con <- alchemer_db(dir)
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)

  expect_true(file.exists(file.path(dir, "catalog.ducklake")))
  expect_true(dir.exists(file.path(dir, "data")))

  tables <- DBI::dbGetQuery(con, "SHOW ALL TABLES")
  expect_true(all(c("raw", "meta") %in% tables$schema))
  expect_true("responses" %in% tables$name[tables$schema == "raw"])
  expect_true("schema_version" %in% tables$name[tables$schema == "meta"])
})

test_that("alchemer_db stamps a fresh catalog at DuckLake spec version >= 1.0", {
  dir <- withr::local_tempdir()
  con <- alchemer_db(dir)
  version <- DBI::dbGetQuery(
    con, "SELECT value FROM __ducklake_metadata_alchemer.ducklake_metadata WHERE key = 'version'"
  )$value
  DBI::dbDisconnect(con, shutdown = TRUE)
  expect_true(package_version(version) >= package_version("1.0"))
})

test_that("reopening an existing database is idempotent", {
  dir <- withr::local_tempdir()
  con1 <- alchemer_db(dir)
  DBI::dbExecute(con1, "INSERT INTO alchemer.raw.surveys (survey_id, title) VALUES ('1', 'Test')")
  DBI::dbDisconnect(con1, shutdown = TRUE)

  con2 <- alchemer_db(dir)
  out <- dplyr::collect(alchemer_tbl(con2, "raw.surveys"))
  DBI::dbDisconnect(con2, shutdown = TRUE)
  expect_equal(nrow(out), 1)
  expect_equal(out$title, "Test")
})

test_that("alchemer_db refuses a catalog stamped below spec version 1.0", {
  dir <- withr::local_tempdir()
  con <- alchemer_db(dir)
  DBI::dbExecute(
    con,
    "UPDATE __ducklake_metadata_alchemer.ducklake_metadata SET value = '0.3' WHERE key = 'version'"
  )
  DBI::dbDisconnect(con, shutdown = TRUE)

  expect_error(alchemer_db(dir), class = "alchemeR_db_error")
  expect_error(alchemer_db(dir), "0.3")
})

test_that("alchemer_db fails fast with an actionable message when the catalog is locked", {
  skip_on_os("windows") # file locking semantics differ; the mechanism is the same everywhere else

  dir <- withr::local_tempdir()
  con1 <- alchemer_db(dir) # holds the lock for the duration of this test
  on.exit(DBI::dbDisconnect(con1, shutdown = TRUE), add = TRUE)

  script <- withr::local_tempfile(fileext = ".R")
  writeLines(c(
    "cat_path <- Sys.getenv('ALCHEMER_TEST_CATALOG')",
    "data_path <- Sys.getenv('ALCHEMER_TEST_DATA')",
    "con <- DBI::dbConnect(duckdb::duckdb())",
    "try(DBI::dbExecute(con, 'INSTALL json'), silent = TRUE)",
    "DBI::dbExecute(con, 'LOAD json')",
    "try(DBI::dbExecute(con, 'INSTALL ducklake'), silent = TRUE)",
    "DBI::dbExecute(con, 'LOAD ducklake')",
    "sql <- paste0(\"ATTACH '\", 'ducklake:', cat_path, \"' AS alchemer (DATA_PATH '\", data_path, \"')\")",
    "res <- tryCatch({DBI::dbExecute(con, sql); 'ok'}, error = function(e) conditionMessage(e))",
    "cat(res)"
  ), script)
  withr::local_envvar(
    ALCHEMER_TEST_CATALOG = file.path(dir, "catalog.ducklake"),
    ALCHEMER_TEST_DATA = paste0(file.path(dir, "data"), "/")
  )
  rscript <- file.path(R.home("bin"), "Rscript")
  out <- system2(rscript, script, stdout = TRUE, stderr = TRUE)
  expect_true(any(grepl("lock", out, ignore.case = TRUE)), info = paste(out, collapse = "\n"))
})

test_that("db_check reports no failures on a clean database", {
  dir <- withr::local_tempdir()
  con <- alchemer_db(dir)
  DBI::dbExecute(con, "INSERT INTO alchemer.raw.surveys (survey_id, title) VALUES ('1', 'Test')")
  DBI::dbExecute(con, "INSERT INTO alchemer.raw.responses (survey_id, response_id) VALUES ('1', 'r1')")
  DBI::dbDisconnect(con, shutdown = TRUE)

  out <- db_check(dir)
  expect_true(all(out$passed))
})

test_that("db_check catches an injected duplicate (survey_id, response_id)", {
  dir <- withr::local_tempdir()
  con <- alchemer_db(dir)
  DBI::dbExecute(con, "INSERT INTO alchemer.raw.surveys (survey_id, title) VALUES ('1', 'Test')")
  DBI::dbExecute(con, "INSERT INTO alchemer.raw.responses (survey_id, response_id) VALUES ('1', 'r1')")
  DBI::dbExecute(con, "INSERT INTO alchemer.raw.responses (survey_id, response_id) VALUES ('1', 'r1')")
  DBI::dbDisconnect(con, shutdown = TRUE)

  out <- db_check(dir)
  expect_false(out$passed[out$check_name == "no_duplicate_responses"])
})

test_that("db_check catches a response with no matching survey", {
  dir <- withr::local_tempdir()
  con <- alchemer_db(dir)
  DBI::dbExecute(con, "INSERT INTO alchemer.raw.responses (survey_id, response_id) VALUES ('missing', 'r1')")
  DBI::dbDisconnect(con, shutdown = TRUE)

  out <- db_check(dir)
  expect_false(out$passed[out$check_name == "responses_reference_known_surveys"])
})

test_that("db_check catches an option with no matching question", {
  dir <- withr::local_tempdir()
  con <- alchemer_db(dir)
  DBI::dbExecute(con, "INSERT INTO alchemer.raw.survey_question_options (survey_id, question_id, option_id) VALUES ('1', 'missing', 'o1')")
  DBI::dbDisconnect(con, shutdown = TRUE)

  out <- db_check(dir)
  expect_false(out$passed[out$check_name == "options_reference_known_questions"])
})

test_that("run_integrity_checks flags a mismatched expected_count", {
  dir <- withr::local_tempdir()
  con <- alchemer_db(dir)
  DBI::dbExecute(con, "INSERT INTO alchemer.raw.surveys (survey_id, title) VALUES ('1', 'Test')")
  DBI::dbExecute(con, "INSERT INTO alchemer.raw.responses (survey_id, response_id) VALUES ('1', 'r1')")

  out <- run_integrity_checks(con, survey_id = "1", expected_count = 2)
  DBI::dbDisconnect(con, shutdown = TRUE)
  expect_false(out$passed[out$check_name == "response_count_matches_fetch"])
})

test_that("alchemer_tbl returns a dbplyr handle onto a raw table", {
  dir <- withr::local_tempdir()
  con <- alchemer_db(dir)
  DBI::dbExecute(con, "INSERT INTO alchemer.raw.surveys (survey_id, title) VALUES ('1', 'Test')")
  out <- dplyr::collect(alchemer_tbl(con, "raw.surveys"))
  DBI::dbDisconnect(con, shutdown = TRUE)
  expect_equal(out$survey_id, "1")
})

test_that("alchemer_db(read_only = TRUE) errors when the database does not exist yet", {
  dir <- withr::local_tempdir()
  expect_error(alchemer_db(file.path(dir, "nope"), read_only = TRUE), class = "alchemeR_db_error")
})

test_that("alchemer_db(read_only = TRUE) can read an existing database", {
  dir <- withr::local_tempdir()
  con <- alchemer_db(dir)
  DBI::dbExecute(con, "INSERT INTO alchemer.raw.surveys (survey_id, title) VALUES ('1', 'Test')")
  DBI::dbDisconnect(con, shutdown = TRUE)

  con2 <- alchemer_db(dir, read_only = TRUE)
  out <- dplyr::collect(alchemer_tbl(con2, "raw.surveys"))
  DBI::dbDisconnect(con2, shutdown = TRUE)
  expect_equal(nrow(out), 1)
})
