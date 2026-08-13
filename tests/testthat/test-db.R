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

test_that("lock_holder_pid extracts the PID DuckDB names, or NA", {
  err <- simpleError(paste(
    'Could not set lock on file "/tmp/x/catalog.ducklake": Conflicting lock is held',
    "in /usr/lib/R/bin/exec/R (PID 714997) by user root."
  ))
  expect_equal(lock_holder_pid(err), 714997L)
  expect_true(is.na(lock_holder_pid(simpleError("Could not set lock on file: unknown holder"))))
})

test_that("a writer.lock naming a process that isn't the lock holder is ignored", {
  # The self-healing property: a writer that crashed leaves writer.lock behind
  # but no longer holds the catalog. Comparing the recorded PID against the PID
  # DuckDB reports as *actually* holding the lock is what keeps that stale file
  # from failing every future run until someone deletes it by hand -- which
  # checking only for the file's existence would do.
  dir <- withr::local_tempdir()
  writeLines("999999", file.path(dir, "writer.lock"))
  expect_false(holder_is_writer(dir, 12345L)) # someone else holds it
  expect_true(holder_is_writer(dir, 999999L)) # the recorded writer holds it
  expect_false(holder_is_writer(dir, NA_integer_)) # DuckDB named nobody
})

test_that("a concurrent alchemeR writer fails fast, and is never waited out or killed", {
  skip_on_os("windows") # file locking semantics differ
  dir <- withr::local_tempdir()
  holder <- local_lock_holder(dir, read_only = FALSE)
  # Stand in for the holder having recorded itself: the subprocess deliberately
  # doesn't load alchemeR (it wouldn't be installed during R CMD check), so the
  # test writes the writer.lock the real code would have written.
  writeLines(as.character(holder$pid), file.path(dir, "writer.lock"))

  before <- Sys.time()
  expect_error(alchemer_db(dir, lock_wait_s = 60), class = "alchemeR_db_locked")
  expect_error(alchemer_db(dir, lock_wait_s = 60), "writer")
  # Both calls returned promptly: lock_wait_s was not slept through.
  expect_lt(as.numeric(difftime(Sys.time(), before, units = "secs")), 30)
  expect_true(process_alive(holder$pid))
})

test_that("a reader still holding the database after the wait is killed, and the writer proceeds", {
  skip_on_os("windows")
  dir <- withr::local_tempdir()
  holder <- local_lock_holder(dir, read_only = TRUE)
  expect_true(process_alive(holder$pid))

  # lock_wait_s = 0 skips the 5-minute wait; every other step is the real path.
  con <- suppressWarnings(alchemer_db(dir, lock_wait_s = 0))
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)

  expect_equal(DBI::dbGetQuery(con, "SELECT COUNT(*) n FROM alchemer.raw.surveys")$n, 0)
  expect_false(process_alive(holder$pid))
  # ...and the writer recorded itself, so the next writer will fail fast
  # rather than killing this one.
  expect_true(holder_is_writer(dir, Sys.getpid()))
})

test_that("break_lock = FALSE leaves the reader alone and aborts instead", {
  skip_on_os("windows")
  dir <- withr::local_tempdir()
  holder <- local_lock_holder(dir, read_only = TRUE)

  expect_error(
    alchemer_db(dir, lock_wait_s = 0, break_lock = FALSE),
    class = "alchemeR_db_locked"
  )
  expect_true(process_alive(holder$pid))
})

test_that("a read-only caller survives a database missing a table added since it was made", {
  # A read-only connection skips ensure_schema() and cannot add anything, so a
  # database last opened by an older version of the package is missing every
  # table added since. Reading one has to degrade to "no rows", not error --
  # db_status() is exactly the sort of thing someone runs first.
  dir <- withr::local_tempdir()
  con <- alchemer_db(dir)
  DBI::dbExecute(con, "DROP TABLE alchemer.meta.loads") # simulate the older schema
  DBI::dbDisconnect(con, shutdown = TRUE)

  con <- alchemer_db(dir, read_only = TRUE)
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  expect_false(table_exists(con, "meta", "loads"))
  expect_true(table_exists(con, "meta", "runs"))
  expect_equal(previously_loaded_tables(con), character(0))
  expect_true(is.na(db_status(dir)$last_load_status))
})

test_that("db_check asserts uniqueness for every table with a natural key", {
  # DuckLake declares no PRIMARY KEY/UNIQUE constraint (ADR-006), so each grain
  # is only known to be unique because it is checked. Responses were checked
  # from the start; surveys, questions, and options were not.
  dir <- withr::local_tempdir()
  con <- alchemer_db(dir)
  DBI::dbExecute(con, "INSERT INTO alchemer.raw.surveys (survey_id, title) VALUES ('1', 'A'), ('1', 'B')")
  DBI::dbExecute(con, "INSERT INTO alchemer.raw.survey_questions (survey_id, question_id)
    VALUES ('1', 'q1'), ('1', 'q1')")
  DBI::dbExecute(con, "INSERT INTO alchemer.raw.survey_question_options
    (survey_id, question_id, option_id) VALUES ('1', 'q1', 'o1'), ('1', 'q1', 'o1')")
  DBI::dbDisconnect(con, shutdown = TRUE)

  out <- db_check(dir)
  failed <- out$check_name[!out$passed]
  expect_true(all(
    c("no_duplicate_surveys", "no_duplicate_questions", "no_duplicate_options") %in% failed
  ))
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
  DBI::dbExecute(con, "
    INSERT INTO alchemer.raw.survey_question_options (survey_id, question_id, option_id)
    VALUES ('1', 'missing', 'o1')")
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
