# The application database is a DuckLake catalog (ADR-001): a DuckDB file
# (`catalog.ducklake`) plus a `data/` directory of Parquet files, attached
# through the `ducklake` extension. Every function in this file is the sole
# way the rest of the package opens that database.

ducklake_alias <- "alchemer"
ducklake_min_version <- "1.0"
data_inlining_row_limit <- 1000L
lock_wait_default_s <- 300

# --- The write lock ---------------------------------------------------------
#
# DuckLake allows one read/write attachment at a time. Two very different
# situations produce the same "Could not set lock on file" error, and they
# want opposite handling:
#
# - **Another writer** (a second ingest(), an overlapping cron tick). Waiting
#   is pointless and killing it would abort real work mid-transaction, so
#   this fails immediately and says so.
# - **A read-only reader** -- an analyst who left `alchemer_db(read_only =
#   TRUE)` open in an IDE session. Readers coexist with each other but block
#   a writer, so a single forgotten session would otherwise fail every
#   scheduled run indefinitely. This waits `lock_wait_s`, and if the same
#   process is still holding on afterwards treats it as stuck and kills it.
#
# The two are told apart by `writer.lock`, which records the PID of the
# process that attached read/write. Crucially, the *file's existence* is not
# what decides: the PID in it is compared against the PID DuckDB names as
# actually holding the lock. A writer that crashed leaves the file behind but
# no longer holds anything, so a stale file self-heals rather than blocking
# every future run until someone deletes it by hand.

writer_lock_path <- function(db) file.path(db, "writer.lock")

is_lock_error <- function(e) {
  grepl("Could not set lock on file", conditionMessage(e), fixed = TRUE)
}

# DuckDB's lock error names the holder: `Conflicting lock is held in
# /path/to/exe (PID 12345) by user root`.
lock_holder_pid <- function(e) {
  found <- regmatches(conditionMessage(e), regexpr("\\(PID [0-9]+\\)", conditionMessage(e)))
  if (length(found) == 0) {
    return(NA_integer_)
  }
  as.integer(gsub("\\D", "", found))
}

holder_is_writer <- function(db, pid) {
  path <- writer_lock_path(db)
  if (is.na(pid) || !file.exists(path)) {
    return(FALSE)
  }
  recorded <- suppressWarnings(as.integer(readLines(path, warn = FALSE)[1]))
  identical(pid, recorded)
}

record_writer_lock <- function(db) {
  writeLines(as.character(Sys.getpid()), writer_lock_path(db))
  invisible(TRUE)
}

# Called by the functions that own a connection's whole lifecycle. Purely
# hygiene -- a leftover file is harmless, per the PID comparison above.
release_writer_lock <- function(db) {
  path <- writer_lock_path(db)
  if (file.exists(path) && holder_is_writer(db, Sys.getpid())) {
    unlink(path)
  }
  invisible(TRUE)
}

# SIGTERM first so the reader's own cleanup runs, SIGKILL only if it ignores
# that. Never our own process, and never a PID DuckDB didn't name.
kill_lock_holder <- function(pid) {
  if (is.na(pid) || identical(pid, Sys.getpid())) {
    return(FALSE)
  }
  cli::cli_warn(c(
    "Killing process {pid}, which has held the application database's read lock
     for longer than the wait budget.",
    "i" = "A read-only reader cannot corrupt the database, so this is safe -- but
           whoever was running it loses their session."
  ))
  tools::pskill(pid, tools::SIGTERM)
  Sys.sleep(2)
  tools::pskill(pid, tools::SIGKILL)
  TRUE
}

# The OS releases the lock when the killed process actually exits, which is
# not instantaneous, so the attach is retried briefly rather than once.
retry_attach_after_kill <- function(try_attach, tries = 5) {
  for (i in seq_len(tries)) {
    err <- try_attach()
    if (is.null(err)) {
      return(TRUE)
    }
    Sys.sleep(1)
  }
  FALSE
}

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
#' @param lock_wait_s How long to wait for a reader to release the database
#'   before giving up (or, if `break_lock`, killing it). A concurrent
#'   *writer* is never waited out: that fails immediately.
#' @param break_lock After `lock_wait_s`, kill the reader still holding the
#'   database and continue. Defaults to `TRUE` for a writer, since a
#'   forgotten read-only session would otherwise fail every scheduled run,
#'   and a reader cannot corrupt anything by being killed. Never kills
#'   another `alchemeR` writer.
#' @return A `DBI` connection with the catalog attached as `alchemer`.
#' @export
alchemer_db <- function(db = alchemer_db_path(), read_only = FALSE,
                        lock_wait_s = lock_wait_default_s, break_lock = !read_only) {
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
  # Named-zone AT TIME ZONE conversions (used by pub_layer() and expunge())
  # need this; DuckDB can autoload it on first use, but loading it
  # explicitly here keeps it in the same offline-pre-staging list as
  # json/ducklake instead of a surprise network fetch mid-query.
  install_and_load(con, "icu")

  options_sql <- if (read_only) {
    "READ_ONLY"
  } else {
    glue::glue("DATA_INLINING_ROW_LIMIT {data_inlining_row_limit}")
  }
  attach_sql <- glue::glue(
    "ATTACH {DBI::dbQuoteString(con, paste0('ducklake:', catalog_path))} AS {ducklake_alias} ",
    "(DATA_PATH {DBI::dbQuoteString(con, paste0(data_dir, '/'))}, {options_sql})"
  )

  attach_catalog(con, attach_sql, db, catalog_path, lock_wait_s, break_lock)
  if (!read_only) {
    record_writer_lock(db)
  }

  assert_ducklake_version(con, catalog_path)
  if (!read_only) {
    ensure_schema(con)
  }
  con
}

# Attach, handling the two lock situations described at the top of this file.
attach_catalog <- function(con, attach_sql, db, catalog_path, lock_wait_s, break_lock) {
  give_up <- function(...) {
    DBI::dbDisconnect(con, shutdown = TRUE)
    cli::cli_abort(..., class = "alchemeR_db_locked", call = NULL)
  }
  try_attach <- function() {
    tryCatch(
      {
        DBI::dbExecute(con, attach_sql)
        NULL
      },
      error = function(e) e
    )
  }

  err <- try_attach()
  if (is.null(err)) {
    return(invisible(TRUE))
  }
  if (!is_lock_error(err)) {
    DBI::dbDisconnect(con, shutdown = TRUE)
    cli::cli_abort(
      c("Failed to open the application database.", "x" = conditionMessage(err)),
      class = "alchemeR_db_error", call = NULL
    )
  }

  pid <- lock_holder_pid(err)
  if (holder_is_writer(db, pid)) {
    give_up(c(
      "Another {.pkg alchemeR} writer (PID {pid}) already holds this application database.",
      "i" = "{.path {catalog_path}}",
      "i" = "Not waited out or interrupted deliberately: it may be partway through a
             survey's refresh transaction, and two concurrent writers means the
             schedule needs more headroom, not a shorter timeout."
    ))
  }

  # Not a writer, so a read-only reader (or a holder DuckDB didn't name).
  if (lock_wait_s > 0) {
    cli::cli_inform(c(
      "Application database is held by a reader{if (is.na(pid)) '' else glue::glue(' (PID {pid})')};
       waiting up to {lock_wait_s}s.",
      "i" = "Readers coexist with each other but block a writer."
    ))
    Sys.sleep(lock_wait_s)
    err <- try_attach()
    if (is.null(err)) {
      return(invisible(TRUE))
    }
    if (!is_lock_error(err)) {
      DBI::dbDisconnect(con, shutdown = TRUE)
      cli::cli_abort(
        c("Failed to open the application database.", "x" = conditionMessage(err)),
        class = "alchemeR_db_error", call = NULL
      )
    }
    pid <- lock_holder_pid(err)
    # Re-checked after the wait, not just before it: the original reader may
    # have disconnected and a *writer* started in the meantime.
    if (holder_is_writer(db, pid)) {
      give_up(c(
        "An {.pkg alchemeR} writer (PID {pid}) took the application database during the wait.",
        "i" = "{.path {catalog_path}}"
      ))
    }
  }

  if (break_lock && kill_lock_holder(pid) && retry_attach_after_kill(try_attach)) {
    return(invisible(TRUE))
  }

  give_up(c(
    "Another process already holds this application database.",
    "i" = "{.path {catalog_path}}",
    "i" = "DuckLake allows one read/write attachment at a time (ADR-001). Any number
           of {.code read_only = TRUE} readers can share it, but a reader blocks a
           writer for as long as it stays connected -- not only while it holds an
           open transaction."
  ))
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
