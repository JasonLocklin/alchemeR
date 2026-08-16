test_that("alchemer_db creates a fresh DuckLake catalog with every schema", {
  dir <- withr::local_tempdir()
  con <- alchemer_db(dir)
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)

  expect_true(file.exists(file.path(dir, "catalog.sqlite")))
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
  out <- DBI::dbGetQuery(con2, "SELECT * FROM alchemer.raw.surveys")
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

test_that("a reader in another process does not block a writer, and vice versa", {
  skip_on_os("windows") # file locking semantics differ
  # This is the whole reason the catalog is SQLite rather than a DuckDB file
  # (ADR-001), and it can only be tested across *processes*: a file lock is
  # per-process, so two attachments from one R session never conflict and an
  # in-process test would pass either way.
  dir <- withr::local_tempdir()
  con <- alchemer_db(dir)
  DBI::dbExecute(con, "INSERT INTO alchemer.raw.surveys (survey_id, title) VALUES ('1', 'S')")
  DBI::dbDisconnect(con, shutdown = TRUE)

  for (holder_mode in c("ro", "rw")) {
    holder <- local_catalog_holder(dir, read_only = (holder_mode == "ro"))
    on.exit(NULL, add = TRUE)

    reader <- alchemer_db(dir, read_only = TRUE)
    expect_equal(DBI::dbGetQuery(reader, "SELECT COUNT(*) n FROM alchemer.raw.surveys")$n, 1)
    DBI::dbDisconnect(reader, shutdown = TRUE)

    writer <- alchemer_db(dir)
    expect_equal(DBI::dbGetQuery(writer, "SELECT COUNT(*) n FROM alchemer.raw.surveys")$n, 1)
    DBI::dbDisconnect(writer, shutdown = TRUE)

    stop_catalog_holder(holder)
  }
})

test_that("the catalog is SQLite and stamped at spec 1.0", {
  dir <- withr::local_tempdir()
  con <- alchemer_db(dir)
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  expect_true(file.exists(file.path(dir, "catalog.sqlite")))
  settings <- DBI::dbGetQuery(con, "FROM ducklake_settings('alchemer')")
  expect_equal(settings$catalog_type, "sqlite")
})

test_that("a directory holding the old DuckDB-file catalog is refused, not silently replaced", {
  # Creating an empty catalog.sqlite beside a populated catalog.ducklake would
  # look exactly like total data loss to whoever ran it.
  dir <- withr::local_tempdir()
  file.create(file.path(dir, "catalog.ducklake"))
  expect_error(alchemer_db(dir), class = "alchemeR_db_error")
  expect_error(alchemer_db(dir), "no longer uses")
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
  out <- DBI::dbGetQuery(con2, "SELECT * FROM alchemer.raw.surveys")
  DBI::dbDisconnect(con2, shutdown = TRUE)
  expect_equal(nrow(out), 1)
})

# --- Application schema versioning (ADR-018) --------------------------------

test_that("a fresh database is stamped with the current major.minor", {
  dir <- withr::local_tempdir()
  con <- alchemer_db(dir)
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  expect_equal(db_schema_version(con), list(major = schema_major, minor = schema_minor))
})

test_that("a database stamped with the pre-split single version reads as major 1", {
  # Databases written before the split hold one `version INTEGER`. Every
  # version stamped under that scheme was a publication-layer change, so N
  # becomes 1.N -- and the caller sees an ordinary minor difference, not an
  # incompatible archive.
  dir <- withr::local_tempdir()
  con <- alchemer_db(dir)
  DBI::dbExecute(con, "DROP TABLE alchemer.meta.schema_version")
  DBI::dbExecute(con, "CREATE TABLE alchemer.meta.schema_version (version INTEGER)")
  DBI::dbExecute(con, "INSERT INTO alchemer.meta.schema_version VALUES (1)")
  DBI::dbDisconnect(con, shutdown = TRUE)

  con <- alchemer_db(dir)
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  expect_equal(db_schema_version(con), list(major = 1L, minor = 1L))
  expect_silent(assert_schema_compatible(con, dir))
})

test_that("reopening a database does not restamp a version that differs from the code", {
  # ensure_schema() must not quietly bring a database up to the current
  # version: the difference is the signal the callers act on.
  dir <- withr::local_tempdir()
  con <- alchemer_db(dir)
  stamp_schema_version(con, 1L, 99L)
  DBI::dbDisconnect(con, shutdown = TRUE)

  con <- alchemer_db(dir)
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  expect_equal(db_schema_version(con)$minor, 99L)
})

test_that("a major version difference stops both writers, in either direction", {
  dir <- withr::local_tempdir()
  con <- alchemer_db(dir)
  stamp_schema_version(con, schema_major + 1L, schema_minor)
  DBI::dbDisconnect(con, shutdown = TRUE)

  expect_error(pub_layer(dir), class = "alchemeR_schema_version_error")
  expect_error(
    ingest(db = dir, client = ingest_test_client()), class = "alchemeR_schema_version_error"
  )

  con <- alchemer_db(dir)
  stamp_schema_version(con, schema_major - 1L, schema_minor)
  DBI::dbDisconnect(con, shutdown = TRUE)
  expect_error(pub_layer(dir), class = "alchemeR_schema_version_error")
})

test_that("a database this refuses to write to can still be inspected", {
  # Being told to archive and rebuild is exactly when someone needs to look
  # inside first, so the assertion lives in the writers, not in the connection.
  dir <- withr::local_tempdir()
  con <- alchemer_db(dir)
  DBI::dbExecute(con, "INSERT INTO alchemer.raw.surveys (survey_id, title) VALUES ('1', 'T')")
  stamp_schema_version(con, schema_major + 1L, schema_minor)
  DBI::dbDisconnect(con, shutdown = TRUE)

  expect_no_error(alchemer_db(dir, read_only = TRUE))
  expect_equal(db_status(dir)$n_surveys, 1)
})
