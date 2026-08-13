# RSQLite stands in for an external analytics database (odbc/SQL Server in
# production) -- load_pub_layer()/load_pipeline_health() only use plain DBI
# calls, so any DBI backend exercises the same code path.

seed_pub_layer <- function(dir) {
  con <- alchemer_db(dir)
  DBI::dbExecute(con, "INSERT INTO alchemer.raw.surveys (survey_id, title, is_deleted) VALUES ('1', 'S1', FALSE)")
  DBI::dbExecute(con, "INSERT INTO alchemer.raw.responses (survey_id, response_id, is_deleted) VALUES
    ('1', 'r1', FALSE), ('1', 'r2', FALSE)")
  DBI::dbDisconnect(con, shutdown = TRUE)
  pub_layer(dir, wide_views = FALSE)
}

test_that("load_pub_layer copies every pub table into the destination", {
  dir <- withr::local_tempdir()
  seed_pub_layer(dir)

  dest <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(dest))

  out <- load_pub_layer(dest, dir)
  expect_true(all(c("surveys", "responses") %in% out$table))
  expect_equal(out$n_rows[out$table == "responses"], 2)

  dest_responses <- DBI::dbGetQuery(dest, "SELECT * FROM responses")
  expect_equal(nrow(dest_responses), 2)
})

test_that("load_pub_layer(tables =) loads only the requested tables", {
  dir <- withr::local_tempdir()
  seed_pub_layer(dir)

  dest <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(dest))

  out <- load_pub_layer(dest, dir, tables = "surveys")
  expect_equal(out$table, "surveys")
  expect_false(DBI::dbExistsTable(dest, "responses"))
})

test_that("load_pub_layer is idempotent and tolerates a schema change between runs", {
  dir <- withr::local_tempdir()
  seed_pub_layer(dir)

  dest <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(dest))

  load_pub_layer(dest, dir, tables = "surveys")
  load_pub_layer(dest, dir, tables = "surveys") # re-run: same result, no error
  expect_equal(nrow(DBI::dbGetQuery(dest, "SELECT * FROM surveys")), 1)

  # simulate a schema change on the source side: a new column appears
  con <- alchemer_db(dir)
  DBI::dbExecute(con, "ALTER TABLE alchemer.pub.surveys ADD COLUMN region VARCHAR")
  DBI::dbExecute(con, "UPDATE alchemer.pub.surveys SET region = 'US'")
  DBI::dbDisconnect(con, shutdown = TRUE)

  load_pub_layer(dest, dir, tables = "surveys")
  dest_cols <- DBI::dbListFields(dest, "surveys")
  expect_true("region" %in% dest_cols)
})

test_that("load_pipeline_health writes a monitorable status row", {
  dir <- withr::local_tempdir()
  seed_pub_layer(dir)

  dest <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(dest))

  out <- load_pipeline_health(dest, dir)
  expect_true(DBI::dbExistsTable(dest, "alchemer_pipeline_health"))
  expect_equal(out$n_surveys, 1)

  loaded <- DBI::dbGetQuery(dest, "SELECT * FROM alchemer_pipeline_health")
  expect_equal(nrow(loaded), 1)
  expect_true("checked_at" %in% names(loaded))
})

test_that("each load records itself in meta.loads, and db_status reports it", {
  dir <- withr::local_tempdir()
  seed_pub_layer(dir)
  dest <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(dest))

  load_pub_layer(dest, dir)

  con <- alchemer_db(dir, read_only = TRUE)
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  loads <- DBI::dbGetQuery(con, "SELECT * FROM alchemer.meta.loads")
  expect_equal(nrow(loads), 1)
  expect_equal(loads$status, "completed")
  expect_true("responses" %in% jsonlite::fromJSON(loads$tables))

  # The destination is write-only, so this is the only place the pipeline can
  # answer "did the last load succeed?" -- and load_pipeline_health() carries
  # it downstream.
  status <- db_status(dir)
  expect_equal(status$last_load_status, "completed")
  expect_gt(status$last_load_tables, 0)
})

test_that("a table this pipeline previously loaded but no longer produces is dropped", {
  # Regression test (code review): renaming a survey changes its wide view's
  # name, so the old destination table was left behind forever -- still
  # queryable, frozen at the last load, with nothing marking it stale. The
  # worst failure shape for a warehouse: dashboards keep working and keep
  # showing old data.
  dir <- withr::local_tempdir()
  seed_pub_layer(dir)
  dest <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(dest))

  con <- alchemer_db(dir)
  DBI::dbExecute(con, "CREATE VIEW alchemer.pub.wide_old_title_1 AS SELECT * FROM alchemer.pub.responses")
  DBI::dbDisconnect(con, shutdown = TRUE)
  load_pub_layer(dest, dir)
  expect_true("wide_old_title_1" %in% DBI::dbListTables(dest))

  # The survey is retitled: pub_layer() drops the old view and makes a new one.
  con <- alchemer_db(dir)
  DBI::dbExecute(con, "DROP VIEW alchemer.pub.wide_old_title_1")
  DBI::dbExecute(con, "CREATE VIEW alchemer.pub.wide_new_title_1 AS SELECT * FROM alchemer.pub.responses")
  DBI::dbDisconnect(con, shutdown = TRUE)
  load_pub_layer(dest, dir)

  expect_true("wide_new_title_1" %in% DBI::dbListTables(dest))
  expect_false("wide_old_title_1" %in% DBI::dbListTables(dest))
})

test_that("only tables a previous load recorded are ever dropped", {
  # The scoping that makes the cleanup safe: a table the pipeline didn't create
  # must survive, however much its name looks like one of ours.
  dir <- withr::local_tempdir()
  seed_pub_layer(dir)
  dest <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(dest))

  DBI::dbWriteTable(dest, "wide_someone_elses_report", data.frame(x = 1))
  load_pub_layer(dest, dir)
  load_pub_layer(dest, dir)
  expect_true("wide_someone_elses_report" %in% DBI::dbListTables(dest))
})

test_that("a failed load is recorded as failed and raises", {
  dir <- withr::local_tempdir()
  seed_pub_layer(dir)
  dest <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  DBI::dbDisconnect(dest) # closed underneath us: every write will fail

  expect_error(load_pub_layer(dest, dir), class = "alchemeR_load_error")

  con <- alchemer_db(dir, read_only = TRUE)
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  loads <- DBI::dbGetQuery(con, "SELECT status FROM alchemer.meta.loads")
  expect_equal(loads$status, "failed")
  # A failed load must not be mistaken for the last good one when the *next*
  # load decides what to clean up.
  expect_equal(db_status(dir)$last_load_status, "failed")
})

# A DBI connection wrapper that delegates writes to a real connection but
# throws if any read (dbSendQuery, which DBI's default dbGetQuery()/
# dbReadTable() both delegate through) is issued against it -- a genuine
# runtime check of the write-only claim in load.R's file comment, unlike a
# source-text grep for "dest_con"/"dbWriteTable" (which a harmless refactor,
# e.g. renaming the parameter or adding an unrelated dbExistsTable() call,
# could pass or fail for reasons unrelated to the actual guarantee).
methods::setClass(
  "WriteOnlyConnection",
  contains = "DBIConnection",
  representation(inner = "ANY")
)
# setMethod() is given the generic *function object* (DBI::dbWriteTable, not
# the bare string "dbWriteTable") because alchemeR fully-qualifies every DBI
# call rather than importing the package, so the generic isn't otherwise
# resolvable from this scope.
methods::setMethod(DBI::dbWriteTable, "WriteOnlyConnection", function(conn, name, value, ...) {
  DBI::dbWriteTable(conn@inner, name, value, ...)
})
methods::setMethod(DBI::dbExistsTable, "WriteOnlyConnection", function(conn, name, ...) {
  DBI::dbExistsTable(conn@inner, name, ...)
})
methods::setMethod(DBI::dbIsValid, "WriteOnlyConnection", function(dbObj, ...) DBI::dbIsValid(dbObj@inner, ...))
methods::setMethod(DBI::dbDisconnect, "WriteOnlyConnection", function(conn, ...) DBI::dbDisconnect(conn@inner, ...))
methods::setMethod(DBI::dbSendQuery, "WriteOnlyConnection", function(conn, statement, ...) {
  stop("a SELECT was issued against a write-only destination connection: ", statement)
})

test_that("load functions never issue a SELECT against the destination (write-only by design)", {
  dir <- withr::local_tempdir()
  seed_pub_layer(dir)

  real <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  dest <- methods::new("WriteOnlyConnection", inner = real)
  on.exit(DBI::dbDisconnect(dest))

  load_pub_layer(dest, dir) # would throw if it ever read from dest
  load_pipeline_health(dest, dir) # would throw if it ever read from dest

  # sanity check: the wrapper's error path genuinely fires for a real SELECT,
  # so a silently-broken wrapper (e.g. dbSendQuery never dispatching) can't
  # make this test pass vacuously.
  expect_error(DBI::dbGetQuery(dest, "SELECT 1"))
})
