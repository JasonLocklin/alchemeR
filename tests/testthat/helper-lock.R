# Holds a real DuckLake lock from a *separate process*, which is the only way
# to test locking at all: DuckDB's file lock is per-process, so two
# attachments from one R session never conflict and an in-process test would
# report success for something that fails in production.
#
# The subprocess uses plain duckdb calls rather than loading alchemeR, since
# the package isn't necessarily on the subprocess's library path during
# R CMD check. It prints its PID, then holds the attachment until killed.

process_alive <- function(pid) {
  # ps(1) rather than tools::pskill(pid, 0): signal 0 is the usual liveness
  # probe, but tools::pskill() gives no way to read the result.
  isTRUE(system2("ps", c("-p", pid), stdout = FALSE, stderr = FALSE) == 0)
}

# Starts the holder, waits for it to confirm it is attached, and registers
# cleanup with the calling test's environment.
local_lock_holder <- function(db, read_only = FALSE, env = parent.frame()) {
  con <- alchemer_db(db) # create the catalog first
  DBI::dbDisconnect(con, shutdown = TRUE)
  unlink(file.path(db, "writer.lock"))

  script <- tempfile(fileext = ".R")
  out <- tempfile(fileext = ".out")
  options_sql <- if (read_only) ", READ_ONLY" else ""
  writeLines(c(
    "db <- commandArgs(trailingOnly = TRUE)[1]",
    "con <- DBI::dbConnect(duckdb::duckdb())",
    "for (e in c('json', 'ducklake')) {",
    "  try(DBI::dbExecute(con, paste('INSTALL', e)), silent = TRUE)",
    "  DBI::dbExecute(con, paste('LOAD', e))",
    "}",
    sprintf(
      paste0(
        "DBI::dbExecute(con, sprintf(\"ATTACH 'ducklake:%%s/catalog.ducklake' AS a",
        " (DATA_PATH '%%s/data/'%s)\", db, db))"
      ),
      options_sql
    ),
    "cat('ATTACHED', Sys.getpid(), '\\n', sep = ' ')",
    "flush(stdout())",
    "Sys.sleep(600)"
  ), script)

  rscript <- file.path(R.home("bin"), "Rscript")
  system2(rscript, c(script, db), stdout = out, stderr = out, wait = FALSE)

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
    skip(paste("lock holder subprocess did not start:", paste(readLines(out, warn = FALSE), collapse = " ")))
  }

  withr::defer(
    {
      if (process_alive(pid)) tools::pskill(pid, tools::SIGKILL)
      unlink(c(script, out))
    },
    envir = env
  )
  list(pid = pid, out = out)
}
