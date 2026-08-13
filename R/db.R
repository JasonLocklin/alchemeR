# The application database is a DuckLake catalog (ADR-001): a SQLite file
# (`catalog.sqlite`) holding the metadata, plus a `data/` directory of Parquet
# files holding the data, attached through the `ducklake` extension. Every
# function in this file is the sole way the rest of the package opens it.
#
# SQLite, not a DuckDB file, is what makes the database usable by more than one
# process -- which is the difference between "an archive analysts can query"
# and "an archive analysts can query except while the pipeline runs". DuckLake's
# own guidance is explicit about the choice: a DuckDB catalog is "limited to a
# single client", while SQLite is the option for "local data warehousing using
# multiple local clients". Verified across separate processes: with a DuckDB
# catalog a reader blocks a writer outright; with SQLite, readers and writers
# attach freely.
#
# There is consequently no lock to acquire, wait for, or break here. Two
# concurrent writers are possible in principle, and DuckLake arbitrates them
# itself: it retries commits that don't logically conflict, and aborts the ones
# that do. Because ingest() isolates each survey in its own transaction, the
# worst case is that one survey fails this run and refreshes on the next --
# which is how every other transient failure already behaves.

ducklake_alias <- "alchemer"
ducklake_min_version <- "1.0"

# --- Storage tuning, and what was deliberately left alone -------------------
#
# `data_inlining_row_limit` is left at DuckLake's default (10). This used to be
# overridden to 1000, chosen when a refresh wrote a survey's rows wholesale.
# It no longer does anything for `raw`: *verified*, inlining does not apply to
# `MERGE` inserts, which is how every raw row is now written, so raw data always
# lands in Parquet. The default does exactly what is wanted for the tables that
# still use plain INSERT -- `meta.runs`, `run_events`, `integrity_checks`,
# `survey_state`, `loads` all write a handful of rows at a time and land
# in the catalog, where operational records belong.
#
# `target_file_size` is left at the default 512MB. It is the size compaction
# aims for, and at this package's scale the whole archive fits inside one such
# file: *measured*, compaction merged 200 small files into 1. Lowering it would
# fragment storage for no gain.
#
# `raw.responses` is deliberately **not** partitioned. Partitioning by
# survey_id was measured against not partitioning: both pruned a per-survey
# query to a single file read (DuckLake keeps per-file min/max statistics, and
# because a refresh writes one survey per file, those statistics are already
# perfectly selective), and query time was identical within noise. What differed
# was compaction: unpartitioned, 200 files merged into 1; partitioned, into 40
# -- one per partition, permanently fragmenting storage against DuckLake's own
# advice that files be at least a few megabytes. If the archive ever grows large
# enough for a single compacted file to hurt, this is cheap to revisit:
# `ALTER TABLE ... SET PARTITIONED BY (survey_id)` affects only newly written
# data, so no rewrite of existing data is needed.

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
#' stamped spec version is at least 1.0 (ADR-001) -- an older `duckdb` build
#' writes the pre-release 0.3 spec silently, and this refuses to write to (or
#' misread) one. `raw`/`meta`/`pub` schemas and tables are created on first
#' connect and are safe to re-run.
#'
#' Any number of processes may attach at once, readers and writers alike, so
#' an analyst querying the database never blocks a scheduled `ingest()` and is
#' never blocked by one.
#'
#' @param db Application database directory. Defaults to [alchemer_db_path()].
#' @param read_only Attach without creating directories or schema, and without
#'   permission to write -- for analysts who only ever query.
#' @return A `DBI` connection with the catalog attached as `alchemer`.
#' @export
alchemer_db <- function(db = alchemer_db_path(), read_only = FALSE) {
  db <- normalizePath(db, mustWork = FALSE)
  data_dir <- file.path(db, "data")
  catalog_path <- file.path(db, "catalog.sqlite")

  assert_no_legacy_catalog(db, catalog_path)
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
  install_and_load(con, "sqlite")
  install_and_load(con, "ducklake")
  # Named-zone AT TIME ZONE conversions (used by pub_layer() and expunge())
  # need this; DuckDB can autoload it on first use, but loading it
  # explicitly here keeps it in the same offline-pre-staging list as
  # json/sqlite/ducklake instead of a surprise network fetch mid-query.
  install_and_load(con, "icu")

  options_sql <- if (read_only) ", READ_ONLY" else ""
  attach_sql <- glue::glue(
    "ATTACH {DBI::dbQuoteString(con, paste0('ducklake:sqlite:', catalog_path))} ",
    "AS {ducklake_alias} ",
    "(DATA_PATH {DBI::dbQuoteString(con, paste0(data_dir, '/'))}{options_sql})"
  )

  tryCatch(
    DBI::dbExecute(con, attach_sql),
    error = function(e) {
      DBI::dbDisconnect(con, shutdown = TRUE)
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

# A directory holding `catalog.ducklake` was written by an alchemeR before the
# catalog moved to SQLite. There is no in-place conversion, and silently
# creating an empty catalog.sqlite beside it would look like total data loss,
# so this stops and says what to do instead.
assert_no_legacy_catalog <- function(db, catalog_path) {
  legacy <- file.path(db, "catalog.ducklake")
  if (!file.exists(legacy) || file.exists(catalog_path)) {
    return(invisible(TRUE))
  }
  cli::cli_abort(
    c(
      "{.path {db}} holds a DuckDB-file catalog, which this version no longer uses.",
      "i" = "The catalog is now SQLite ({.path catalog.sqlite}), so that analysts can
             read the database while the pipeline writes to it. There is no in-place
             conversion.",
      "i" = "Point {.envvar ALCHEMER_DB} at a fresh directory and re-run {.fn ingest} --
             it rebuilds from the API. Keep the old directory until you have compared
             the two."
    ),
    class = "alchemeR_db_error", call = NULL
  )
}

# The version stamp lives in the catalog's own metadata tables, which are
# not exposed through the `alchemer` alias itself -- DuckLake attaches the
# physical catalog internally under `__ducklake_metadata_<alias>`, and that
# hidden attachment is queryable directly.
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
