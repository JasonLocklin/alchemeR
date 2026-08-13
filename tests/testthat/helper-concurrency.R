# Holds the catalog open from a *separate process*, which is the only way to
# test concurrency: DuckDB/SQLite locking is per-process, so two attachments
# from one R session never conflict and an in-process test would pass whatever
# the catalog backend does.
#
# The subprocess uses plain duckdb calls rather than loading alchemeR, since
# the package isn't necessarily on the subprocess's library path during
# R CMD check.

process_alive <- function(pid) {
  isTRUE(system2("ps", c("-p", pid), stdout = FALSE, stderr = FALSE) == 0)
}

local_catalog_holder <- function(db, read_only = FALSE) {
  script <- tempfile(fileext = ".R")
  out <- tempfile(fileext = ".out")
  options_sql <- if (read_only) ", READ_ONLY" else ""
  writeLines(c(
    "db <- commandArgs(trailingOnly = TRUE)[1]",
    "con <- DBI::dbConnect(duckdb::duckdb())",
    "for (e in c('json', 'sqlite', 'ducklake')) {",
    "  try(DBI::dbExecute(con, paste('INSTALL', e)), silent = TRUE)",
    "  DBI::dbExecute(con, paste('LOAD', e))",
    "}",
    sprintf(
      paste0(
        "DBI::dbExecute(con, sprintf(\"ATTACH 'ducklake:sqlite:%%s/catalog.sqlite' AS a",
        " (DATA_PATH '%%s/data/'%s)\", db, db))"
      ),
      options_sql
    ),
    "cat('ATTACHED', Sys.getpid(), '\\n', sep = ' ')",
    "flush(stdout())",
    "Sys.sleep(300)"
  ), script)

  system2(file.path(R.home("bin"), "Rscript"), c(script, db),
          stdout = out, stderr = out, wait = FALSE)

  pid <- NA_integer_
  for (i in 1:120) {
    Sys.sleep(0.5)
    lines <- if (file.exists(out)) readLines(out, warn = FALSE) else character(0)
    hit <- grep("^ATTACHED ", lines, value = TRUE)
    if (length(hit) > 0) {
      pid <- as.integer(sub("^ATTACHED ", "", hit[1]))
      break
    }
  }
  if (is.na(pid)) {
    testthat::skip(paste(
      "catalog holder subprocess did not start:",
      paste(readLines(out, warn = FALSE), collapse = " ")
    ))
  }
  list(pid = pid, script = script, out = out)
}

stop_catalog_holder <- function(holder) {
  if (process_alive(holder$pid)) {
    tools::pskill(holder$pid, tools::SIGKILL)
  }
  unlink(c(holder$script, holder$out))
  invisible(TRUE)
}
