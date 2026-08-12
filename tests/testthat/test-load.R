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

test_that("load functions never issue a SELECT against the destination (write-only by design)", {
  # Structural check on the write-only claim in load.R's file comment: the
  # only calls made against dest_con are dbWriteTable() (create/insert).
  src <- c(deparse(body(load_pub_layer)), deparse(body(load_pipeline_health)), deparse(body(destination_name)))
  dest_calls <- grep("dest_con", src, value = TRUE)
  expect_true(length(dest_calls) > 0)
  expect_true(all(grepl("dbWriteTable", dest_calls)))
})
