# The "L" of the pipeline: copying the publication layer into an external
# analytics database. Deliberately thin and backend-agnostic -- every function
# here takes a plain DBI connection, so the same code works against SQL Server
# (via odbc), Postgres, SQLite, or anything else DBI supports. The package
# depends on no specific driver; inst/scripts/etl_pipeline.R shows odbc + SQL
# Server as one concrete example.
#
# Every load is a full overwrite (DBI::dbWriteTable(overwrite = TRUE)), never
# an incremental upsert -- mirroring ingest()'s own full-refresh philosophy
# (ADR-004) and, as a consequence, tolerating schema changes on either side by
# construction: a table is dropped and recreated from whatever pub_layer()
# currently produces, not merged into a fixed destination shape.
#
# The destination is treated as write-only: nothing here reads it back to
# decide what to do, so its service account needs only CREATE/DROP/INSERT, and
# never SELECT. All state lives in the local application database, in
# meta.loads (see db_schema.R).
#
# Trade-off, stated plainly: a concurrent reader of the destination can see a
# table briefly empty or missing while it is being overwritten. An atomic
# staging-table swap would remove that gap, but rename syntax differs enough
# across SQL dialects (SQL Server's sp_rename vs ALTER TABLE ... RENAME TO) to
# make a generic version real, hard-to-audit complexity for a narrow window.
# Schedule loads for low-read periods, or have consumers check the health
# table's `checked_at` before trusting a table's contents.

#' Load the publication layer into an external analytics database
#'
#' Copies every `pub.*` table (including generated wide views) into
#' `dest_con` via [DBI::dbWriteTable()] with `overwrite = TRUE` -- a full
#' replace per table, never an incremental update.
#'
#' Destination tables this pipeline wrote on a previous run and no longer
#' produces are dropped, so a retitled survey doesn't leave its old
#' `wide_<slug>_<id>` table behind as a stale, still-queryable copy. Only
#' names recorded in `meta.loads` are ever dropped -- tables the pipeline
#' didn't create are never touched.
#'
#' @param dest_con A `DBI` connection to the destination database.
#' @param db Application database directory. Defaults to [alchemer_db_path()].
#' @param tables Which `pub` tables/views to load. Defaults to every one present.
#' @param schema Destination schema name (e.g. `"dbo"` for SQL Server).
#'   `NULL` uses the destination's default schema.
#' @return Invisibly, a tibble: one row per table loaded, with row counts and
#'   timing.
#' @export
load_pub_layer <- function(dest_con, db = alchemer_db_path(), tables = NULL, schema = NULL) {
  load_id <- new_run_id()
  started_at <- Sys.time()

  # Read-only for the copy itself, which is the long part: an analyst reading
  # the application database is not blocked by it, and it cannot block the
  # next ingest() for any longer than the copy takes. meta.loads is written
  # afterwards over a separate, short-lived read/write connection.
  con <- alchemer_db(db, read_only = TRUE)
  available <- DBI::dbGetQuery(con, glue::glue(
    "SELECT table_name FROM information_schema.tables
     WHERE table_catalog = '{ducklake_alias}' AND table_schema = 'pub'"
  ))$table_name
  targets <- if (is.null(tables)) available else intersect(as.character(tables), available)
  previous <- previously_loaded_tables(con)
  DBI::dbDisconnect(con, shutdown = TRUE)

  outcome <- tryCatch(
    {
      rows <- purrr::map(targets, function(tbl) {
        t0 <- Sys.time()
        con <- alchemer_db(db, read_only = TRUE)
        on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
        data <- DBI::dbGetQuery(con, glue::glue("SELECT * FROM {ducklake_alias}.pub.{tbl}"))
        DBI::dbWriteTable(dest_con, destination_name(schema, tbl), data, overwrite = TRUE)
        tibble::tibble(
          table = tbl, n_rows = nrow(data), loaded_at = Sys.time(),
          duration_s = as.numeric(difftime(Sys.time(), t0, units = "secs"))
        )
      })
      # Only after every table is written: dropping first would leave a gap if
      # the load then failed, and dropping a table still being produced would
      # be a bug rather than a cleanup.
      drop_stale_destination_tables(dest_con, schema, setdiff(previous, targets))
      list(status = "completed", message = NA_character_,
           rows = if (length(rows) == 0) empty_load_result() else dplyr::bind_rows(rows))
    },
    error = function(e) list(status = "failed", message = conditionMessage(e), rows = empty_load_result())
  )

  record_load(
    db, load_id, started_at, outcome$status, outcome$message,
    destination = destination_label(dest_con), tables = outcome$rows$table,
    n_rows = sum(outcome$rows$n_rows)
  )
  if (identical(outcome$status, "failed")) {
    cli::cli_abort(
      c("Loading the publication layer failed.", "x" = outcome$message),
      class = "alchemeR_load_error", call = NULL
    )
  }
  invisible(outcome$rows)
}

empty_load_result <- function() {
  tibble::tibble(
    table = character(0), n_rows = integer(0),
    loaded_at = Sys.time()[0], duration_s = numeric(0)
  )
}

destination_name <- function(schema, table) {
  if (is.null(schema)) table else DBI::Id(schema = schema, table = table)
}

# For the meta.loads audit trail only -- never credentials, which several
# backends embed in the connection string.
destination_label <- function(dest_con) {
  paste(class(dest_con)[1], collapse = "/")
}

previously_loaded_tables <- function(con) {
  if (!table_exists(con, "meta", "loads")) {
    return(character(0))
  }
  last <- DBI::dbGetQuery(con, glue::glue(
    "SELECT tables FROM {ducklake_alias}.meta.loads
     WHERE status = 'completed' ORDER BY started_at DESC LIMIT 1"
  ))$tables
  if (length(last) == 0 || is.na(last[1])) {
    return(character(0))
  }
  as.character(jsonlite::fromJSON(last[1]))
}

# Scoped to names a previous successful load recorded: the pipeline drops only
# what it created. A pattern-based sweep (`wide_%`) could delete a table
# someone else owns.
drop_stale_destination_tables <- function(dest_con, schema, stale) {
  for (tbl in stale) {
    try(DBI::dbRemoveTable(dest_con, destination_name(schema, tbl)), silent = TRUE)
  }
  invisible(stale)
}

record_load <- function(db, load_id, started_at, status, message, destination, tables, n_rows) {
  con <- alchemer_db(db)
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  write_rows_generic(con, "meta.loads", tibble::tibble(
    load_id = load_id, started_at = started_at, finished_at = Sys.time(),
    status = status, destination = destination,
    n_tables = length(tables), n_rows = as.integer(or_default(n_rows, 0L)),
    tables = as.character(jsonlite::toJSON(tables)),
    message = as.character(message)
  ))
}

#' Load a pipeline-health summary into the analytics database
#'
#' Writes [db_status()] into the destination as a plain table, so tools
#' monitoring the analytics database can see whether the pipeline is current
#' and healthy without needing any access back to the application database.
#'
#' Call it *after* [load_pub_layer()] so the load's own outcome is included.
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
