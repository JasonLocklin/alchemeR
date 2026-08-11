# The application database is a DuckLake catalog (ADR-001): a DuckDB file
# (`catalog.ducklake`) plus a `data/` directory of Parquet files, attached
# through the `ducklake` extension. Every function in this file is the sole
# way the rest of the package opens that database.

ducklake_alias <- "alchemer"
ducklake_min_version <- "1.0"
data_inlining_row_limit <- 1000L

# Best-effort: `INSTALL` needs network access once; if the extension is
# already present (e.g. pre-staged for an air-gapped install), a failed
# INSTALL is not fatal as long as LOAD succeeds.
install_and_load <- function(con, extension) {
  tryCatch(DBI::dbExecute(con, glue::glue("INSTALL {extension}")), error = function(e) NULL)
  DBI::dbExecute(con, glue::glue("LOAD {extension}"))
}

#' Open (or create) the alchemeR application database
#'
#' Attaches the DuckLake catalog at `db` as `alchemer`, asserting that its
#' stamped spec version is at least 1.0 (ADR-001) --
#' an older `duckdb` build writes the pre-release 0.3 spec silently, and this
#' refuses to write to (or misread) one. `raw`/`meta`/`pub` schemas and
#' tables are created on first connect and are safe to re-run.
#'
#' @param db Application database directory. Defaults to [alchemer_db_path()].
#' @param read_only Open without creating directories, schema, or the
#'   read/write lock DuckLake normally holds while attached (ADR-001) --
#'   for analysts who only ever query the database.
#' @return A `DBI` connection with the catalog attached as `alchemer`.
#' @export
alchemer_db <- function(db = alchemer_db_path(), read_only = FALSE) {
  db <- normalizePath(db, mustWork = FALSE)
  data_dir <- file.path(db, "data")
  catalog_path <- file.path(db, "catalog.ducklake")

  if (read_only) {
    if (!file.exists(catalog_path)) {
      cli::cli_abort(
        "No application database found at {.path {db}}.",
        class = "alchemeR_db_error"
      )
    }
  } else {
    dir.create(data_dir, recursive = TRUE, showWarnings = FALSE)
  }

  con <- suppressMessages(DBI::dbConnect(duckdb::duckdb()))
  install_and_load(con, "json")
  install_and_load(con, "ducklake")

  options_sql <- if (read_only) {
    "READ_ONLY"
  } else {
    glue::glue("DATA_INLINING_ROW_LIMIT {data_inlining_row_limit}")
  }
  attach_sql <- glue::glue(
    "ATTACH {DBI::dbQuoteString(con, paste0('ducklake:', catalog_path))} AS {ducklake_alias} ",
    "(DATA_PATH {DBI::dbQuoteString(con, paste0(data_dir, '/'))}, {options_sql})"
  )

  tryCatch(
    DBI::dbExecute(con, attach_sql),
    error = function(e) {
      DBI::dbDisconnect(con, shutdown = TRUE)
      if (grepl("Could not set lock on file", conditionMessage(e), fixed = TRUE)) {
        cli::cli_abort(
          c(
            "Another process already holds this application database.",
            "i" = "{.path {catalog_path}}",
            "i" = "Only one writer may hold the DuckLake catalog at a time (ADR-001).
                   Wait for the other run to finish, then retry."
          ),
          class = "alchemeR_db_locked", call = NULL
        )
      }
      cli::cli_abort(
        c("Failed to open the application database.", "x" = conditionMessage(e)),
        class = "alchemeR_db_error", call = NULL
      )
    }
  )

  assert_ducklake_version(con, catalog_path)
  if (!read_only) {
    ensure_schema(con)
  }
  con
}

# The version stamp lives in the catalog's own metadata tables, which are
# not exposed through the `alchemer` alias itself -- DuckLake attaches the
# physical catalog file internally under `__ducklake_metadata_<alias>`,
# and that hidden attachment is queryable directly (verified in plan.md §2).
assert_ducklake_version <- function(con, catalog_path) {
  meta_alias <- paste0("__ducklake_metadata_", ducklake_alias)
  version <- DBI::dbGetQuery(
    con,
    glue::glue("SELECT value FROM {meta_alias}.ducklake_metadata WHERE key = 'version'")
  )$value

  if (length(version) == 0 || package_version(version) < package_version(ducklake_min_version)) {
    DBI::dbDisconnect(con, shutdown = TRUE)
    cli::cli_abort(
      c(
        "The catalog at {.path {catalog_path}} uses DuckLake spec version
         {version %||% 'unknown'}, older than the required {ducklake_min_version}.",
        "i" = "This usually means it was created with duckdb < 1.5.2. Recreate it with
               a newer duckdb, or point {.envvar ALCHEMER_DB} at a fresh directory."
      ),
      class = "alchemeR_db_error", call = NULL
    )
  }
  invisible(TRUE)
}

#' A dbplyr handle onto an application database table
#'
#' @param con A connection from [alchemer_db()].
#' @param table Schema-qualified table name, e.g. `"raw.responses"`.
#' @return A `tbl` for use with dplyr verbs.
#' @export
alchemer_tbl <- function(con, table) {
  # dplyr::tbl() on a DBI connection dispatches through dbplyr, a genuine
  # runtime dependency of this function even though it is never called
  # directly elsewhere in this package.
  dbplyr::dbplyr_edition()
  dplyr::tbl(con, I(paste0(ducklake_alias, ".", table)))
}
