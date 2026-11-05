# Pull once, then make the saved file look old so the next call checks the API.
first_pull <- function(n = 5, env = parent.frame()) {
  dir <- withr::local_tempdir(.local_envir = env)
  calls <- fake_account(n, env)
  suppressMessages(fetch_survey(123, dir = dir))
  list(dir = dir, calls = calls, file = file.path(dir, "survey_123.rds"))
}
age_file <- function(x) Sys.setFileTime(x$file, Sys.time() - 3600)
refetch <- function(x, ...) {
  suppressMessages(fetch_survey(123, dir = x$dir, ...))
}
n_requests <- function(x) length(x$calls$reqs)
paths <- function(x) vapply(x$calls$reqs, \(r) httr2::url_parse(r$url)$path, "")

test_that("an inactive survey costs 2 requests, no rewrite, and refreshes the mtime", {
  x <- first_pull()
  age_file(x)
  before <- unname(tools::md5sum(x$file))
  n <- n_requests(x)
  expect_message(
    out <- fetch_survey(123, dir = x$dir),
    "unchanged .*, 2 requests"
  )
  expect_equal(n_requests(x) - n, 2)
  expect_equal(unname(tools::md5sum(x$file)), before)
  expect_lt(as.numeric(Sys.time() - file.mtime(x$file), units = "secs"), 60)
  expect_equal(nrow(out), 5)
})

test_that("new and edited responses are pulled from a UTC watermark", {
  x <- first_pull()
  age_file(x)
  x$calls$acct$add(2) # ids 6, 7
  x$calls$acct$rows[["2"]]$date_updated <- "2026-11-01 07:00:00" # edited, newest
  x$calls$acct$rows[["2"]]$status <- "Partial"
  expect_message(
    out <- fetch_survey(123, dir = x$dir),
    "incremental pull \\(3 rows changed\\)"
  )
  expect_equal(out$id, 1:7)
  expect_equal(out$status[out$id == 2], "Partial")
  q <- query_of(x$calls$reqs[[n_requests(x) - 1]]) # the pull, before the count check
  # newest stored update was id 5 at 05:05:00 UTC; the window starts 2 days earlier
  expect_equal(
    c(q$`filter[field][1]`, q$`filter[operator][1]`),
    c("date_updated", ">=")
  )
  expect_equal(q$`filter[value][1]`, "2026-10-30 05:05:00")
})

test_that("labels, types and metadata survive a merge", {
  x <- first_pull()
  age_file(x)
  x$calls$acct$add(1)
  out <- refetch(x)
  expect_equal(attr(out$Q3, "label"), "Q3 colour ?")
  expect_equal(attr(out$Q4_10005, "label"), "Fruit: Apple")
  expect_type(out$Q16, "double")
  expect_equal(attr(out, "alchemer")$total_count, 6)
  expect_true("Q3" %in% attr(out, "alchemer")$dictionary$question_id)
  expect_identical(readRDS(x$file), out)
})

# 3 responses older than the 2-day window and 2 recent ones
two_ages <- function(env = parent.frame()) {
  dir <- withr::local_tempdir(.local_envir = env)
  calls <- fake_account(0, env)
  calls$acct$add(3, minutes = -5000)
  calls$acct$add(2, minutes = 1)
  suppressMessages(fetch_survey(123, dir = dir))
  list(dir = dir, calls = calls, file = file.path(dir, "survey_123.rds"))
}

test_that("a response deleted inside the window disappears", {
  x <- first_pull()
  age_file(x)
  x$calls$acct$rows[["3"]] <- NULL
  out <- refetch(x)
  expect_equal(out$id, c(1, 2, 4, 5))
})

test_that("a response that moved into the window is not duplicated", {
  x <- two_ages()
  age_file(x)
  x$calls$acct$rows[["1"]]$date_updated <- "2026-11-01 06:00:00" # old row edited
  out <- refetch(x)
  expect_equal(sort(out$id), 1:5)
  expect_equal(anyDuplicated(out$id), 0)
})

test_that("a deletion outside the window forces a full refresh", {
  x <- two_ages()
  age_file(x)
  x$calls$acct$rows[["1"]] <- NULL
  x$calls$acct$add(1, minutes = 10) # activity, so the probe sees a change
  expect_message(
    out <- fetch_survey(123, dir = x$dir),
    "full refresh needed \\(older responses changed\\)"
  )
  expect_equal(out$id, c(2, 3, 4, 5, 6))
})

test_that("each other trigger forces a full refresh", {
  x <- first_pull()
  age_file(x)

  x$calls$acct$modified_on <- "2026-10-02 09:00:00" # the survey was edited
  expect_message(
    refetch_out <- fetch_survey(123, dir = x$dir),
    "survey was edited"
  )
  expect_equal(attr(refetch_out, "alchemer")$modified_on, "2026-10-02 09:00:00")

  age_file(x)
  expect_message(
    fetch_survey(123, dir = x$dir, include_test = TRUE),
    "include_test changed"
  )

  age_file(x)
  old <- readRDS(x$file)
  attr(old, "alchemer")$schema_version <- 0L
  saveRDS(old, x$file)
  age_file(x)
  expect_message(
    fetch_survey(123, dir = x$dir, include_test = TRUE),
    "file format changed"
  )
})

test_that("only re-pulled rows get a new fetched_at", {
  x <- two_ages()
  before <- readRDS(x$file)
  Sys.sleep(0.05)
  age_file(x)
  x$calls$acct$add(1, minutes = 10)
  out <- refetch(x)
  old <- out$id %in% 1:3 # outside the window: kept as they were
  expect_equal(out$fetched_at[old], before$fetched_at[before$id %in% 1:3])
  expect_true(all(out$fetched_at[!old] > max(before$fetched_at)))
})

test_that("the full-refresh sweep needs both an old file and activity", {
  x <- two_ages()
  old <- readRDS(x$file)
  old$fetched_at <- old$fetched_at - 100 * 86400
  saveRDS(old, x$file)

  # inactive survey: never swept
  age_file(x)
  expect_message(fetch_survey(123, dir = x$dir), "unchanged")

  # active survey: swept
  x$calls$acct$add(1, minutes = 10)
  age_file(x)
  expect_message(
    fetch_survey(123, dir = x$dir),
    "full refresh needed \\(the oldest rows are over 90 days old\\)"
  )
})

test_that("an incremental pull with no rows keeps every column", {
  x <- first_pull()
  age_file(x)
  x$calls$acct$rows <- x$calls$acct$rows[1:4] # id 5 deleted; nothing new in the window
  x$calls$acct$rows[["1"]] <- NULL
  out <- refetch(x)
  expect_equal(out$id, 2:4)
  expect_true(all(c("Q3", "Q6", "Q13", "fetched_at") %in% names(out)))
  expect_type(out$Q16, "double")
})

test_that("a failed incremental update leaves the file and its age untouched", {
  x <- first_pull()
  age_file(x)
  before <- unname(tools::md5sum(x$file))
  mtime <- file.mtime(x$file)
  x$calls$acct$add(1)
  mock_api(\(req) {
    q <- query_of(req)
    path <- httr2::url_parse(req$url)$path
    if (!is.null(q$`filter[field][1]`) && grepl("surveyresponse$", path)) {
      json(list(result_ok = FALSE), 400)
    } else if (grepl("surveyresponse$", path)) {
      page_response(req, 1, \(i) {
        response(5, full_answers(), date_updated = "2026-11-02 00:00:00")
      })
    } else {
      json(list(result_ok = TRUE, data = definition()))
    }
  })
  expect_error(fetch_survey(123, dir = x$dir), "HTTP 400")
  expect_equal(unname(tools::md5sum(x$file)), before)
  expect_equal(file.mtime(x$file), mtime)
})

test_that("a survey that was empty gets a full pull when responses arrive", {
  dir <- withr::local_tempdir()
  calls <- fake_account(0)
  out <- suppressMessages(fetch_survey(123, dir = dir))
  expect_equal(nrow(out), 0)
  Sys.setFileTime(file.path(dir, "survey_123.rds"), Sys.time() - 3600)
  calls$acct$add(2)
  expect_message(out <- fetch_survey(123, dir = dir), "first responses")
  expect_equal(nrow(out), 2)
})

test_that("an empty survey stays empty with 2 requests and no rewrite", {
  dir <- withr::local_tempdir()
  calls <- fake_account(0)
  suppressMessages(fetch_survey(123, dir = dir))
  file <- file.path(dir, "survey_123.rds")
  Sys.setFileTime(file, Sys.time() - 3600)
  before <- unname(tools::md5sum(file))
  n <- length(calls$reqs)
  expect_message(
    out <- fetch_survey(123, dir = dir),
    "unchanged \\(no responses\\), 2 requests"
  )
  expect_equal(length(calls$reqs) - n, 2)
  expect_equal(unname(tools::md5sum(file)), before)
  expect_equal(nrow(out), 0)
})
