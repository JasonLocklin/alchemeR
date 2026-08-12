# The "L" of an ETL pipeline built on top of ingest()/pub_layer(): copying
# the publication layer into an external analytics database. Deliberately
# thin and backend-agnostic -- every function here takes a plain DBI
# connection, so the same code works against SQL Server (via odbc), Postgres,
# SQLite, or anything else DBI supports. The package does not depend on any
# specific backend driver; inst/scripts/etl_pipeline.R shows odbc + SQL
# Server as one concrete example.
#
# Every load is a full overwrite (DBI::dbWriteTable(overwrite = TRUE)), never
# an incremental upsert -- mirroring ingest()'s own full-refresh philosophy
# (ADR-004) and, as a consequence, naturally tolerating schema changes on
# either side: a table is dropped and recreated from whatever pub_layer()
# currently produces, not merged into a fixed destination shape. This also
# means the destination service account only ever needs
# create/drop/insert privileges -- never SELECT -- because nothing here
# reads the destination back to decide what to do; all state (what to load,
# whether the last run succeeded) lives in the local application database.
#
# Trade-off, stated plainly: a concurrent reader of the destination can see
# a table briefly empty or missing while it's being overwritten. This is a
# deliberate simplicity choice -- an atomic staging-table swap would remove
# that gap, but table-rename syntax differs enough across SQL dialects
# (SQL Server's sp_rename vs. ALTER TABLE ... RENAME TO elsewhere) that
# doing it generically would add real, hard-to-audit complexity for a
# narrow window. Schedule loads for low-read periods, or have downstream
# consumers check `loaded_at` in the health table below before trusting a
# table's contents.

#' Load the publication layer into an external analytics database
#'
#' Copies every `pub.*` table (including generated wide views) from the
#' local application database into `dest_con` via [DBI::dbWriteTable()]
#' with `overwrite = TRUE` -- a full replace per table, never an
#' incremental update.
#'
#' @param dest_con A `DBI` connection to the destination database.
#' @param db Application database directory. Defaults to [alchemer_db_path()].
#' @param tables Which `pub` tables/views to load. Defaults to every one present.
#' @param schema Destination schema name (e.g. `"dbo"` for SQL Server).
#'   `NULL` uses the destination's default schema.
#' @return Invisibly, a tibble: one row per table loaded, with row counts and timing.
#' @export
load_pub_layer <- function(dest_con, db = alchemer_db_path(), tables = NULL, schema = NULL) {
  con <- alchemer_db(db, read_only = TRUE)
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))

  available <- DBI::dbGetQuery(con, glue::glue(
    "SELECT table_name FROM information_schema.tables
     WHERE table_catalog = '{ducklake_alias}' AND table_schema = 'pub'"
  ))$table_name
  targets <- if (is.null(tables)) available else intersect(as.character(tables), available)

  rows <- purrr::map(targets, function(tbl) {
    t0 <- Sys.time()
    data <- DBI::dbGetQuery(con, glue::glue("SELECT * FROM {ducklake_alias}.pub.{tbl}"))
    DBI::dbWriteTable(dest_con, destination_name(schema, tbl), data, overwrite = TRUE)
    tibble::tibble(
      table = tbl, n_rows = nrow(data), loaded_at = Sys.time(),
      duration_s = as.numeric(difftime(Sys.time(), t0, units = "secs"))
    )
  })
  invisible(if (length(rows) == 0) tibble::tibble() else dplyr::bind_rows(rows))
}

destination_name <- function(schema, table) {
  if (is.null(schema)) table else DBI::Id(schema = schema, table = table)
}

#' Load a pipeline-health summary into the analytics database
#'
#' Writes [db_status()] into the destination as a plain table, so tools
#' monitoring the analytics database can see whether the pipeline is
#' current and healthy without needing any access back to the local
#' application database.
#'
#' @inheritParams load_pub_layer
#' @param table Destination table name.
#' @return Invisibly, the health row that was loaded (a one-row tibble).
#' @export
load_pipeline_health <- function(dest_con, db = alchemer_db_path(),
                                 table = "alchemer_pipeline_health", schema = NULL) {
  status <- db_status(db)
  status$checked_at <- Sys.time()
  DBI::dbWriteTable(dest_con, destination_name(schema, table), status, overwrite = TRUE)
  invisible(status)
}
