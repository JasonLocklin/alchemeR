test_that("all_surveys returns tidy columns and errors above one page", {
  survey_api(1)
  out <- all_surveys()
  expect_named(
    out,
    c("id", "title", "status", "type", "team", "created_on", "modified_on")
  )
  expect_equal(out$team, "1,2")
  expect_s3_class(out$modified_on, "POSIXct")

  mock_api(\(req) {
    json(list(
      result_ok = TRUE,
      total_count = 900,
      total_pages = 2,
      data = list()
    ))
  })
  expect_error(all_surveys(), "more than 500")
})

test_that("a full pull returns a labelled tibble and writes one RDS file", {
  dir <- withr::local_tempdir()
  survey_api(600)
  expect_message(
    out <- fetch_survey(123, dir = dir),
    "Survey 123: full pull \\(no usable local file\\), 600 rows, 4 requests"
  )
  expect_equal(nrow(out), 600)
  expect_equal(names(out)[1], "survey_id")
  expect_equal(names(out)[ncol(out)], "fetched_at")
  expect_equal(out$id, 1:600)
  expect_equal(attr(out$Q3, "label"), "Q3 colour ?")
  meta <- attr(out, "alchemer")
  expect_equal(meta$survey_id, 123L)
  expect_equal(meta$total_count, 600)
  expect_equal(meta$tz, "America/Toronto")
  expect_true("Q3" %in% meta$dictionary$question_id)
  expect_equal(dir(dir), "survey_123.rds")
  expect_identical(readRDS(file.path(dir, "survey_123.rds")), out)
})

test_that("rows are sorted by id even when the API returns them out of order", {
  dir <- withr::local_tempdir()
  survey_api(3, ids = c(30, 10, 20))
  expect_equal(suppressMessages(fetch_survey(123, dir = dir))$id, c(10, 20, 30))
})

test_that("a failure mid-pull leaves the existing file byte-identical", {
  dir <- withr::local_tempdir()
  survey_api(600)
  suppressMessages(fetch_survey(123, dir = dir))
  before <- unname(tools::md5sum(file.path(dir, "survey_123.rds")))
  mock_api(\(req) {
    path <- httr2::url_parse(req$url)$path
    if (grepl("surveyresponse$", path) && query_of(req)$page == "2") {
      json(list(result_ok = FALSE), 400)
    } else if (grepl("surveyresponse$", path)) {
      page_response(req, 600, \(i) response(i, full_answers()))
    } else {
      json(list(result_ok = TRUE, data = definition()))
    }
  })
  expect_error(fetch_survey(123, dir = dir, refresh = "full"), "HTTP 400")
  expect_equal(unname(tools::md5sum(file.path(dir, "survey_123.rds"))), before)
  expect_equal(dir(dir), "survey_123.rds") # no temp files left behind
})

test_that("an unwritable target returns the data with a warning", {
  dir <- withr::local_tempdir()
  dir.create(file.path(dir, "survey_123.rds")) # a folder where the file belongs
  survey_api(3)
  expect_warning(
    out <- suppressMessages(fetch_survey(123, dir = dir)),
    "Could not save"
  )
  expect_equal(nrow(out), 3)
  expect_true(dir.exists(file.path(dir, "survey_123.rds")))
  expect_equal(dir(dir), "survey_123.rds")
})

test_that("two surveys share one folder, each in its own file", {
  dir <- withr::local_tempdir()
  survey_api(2)
  suppressMessages(fetch_survey(123, dir = dir))
  file.copy(file.path(dir, "survey_123.rds"), file.path(dir, "survey_456.rds"))
  expect_error(
    fetch_survey(456, dir = dir, refresh = "local"),
    "not a readable file for survey 456"
  )
  expect_equal(sort(dir(dir)), c("survey_123.rds", "survey_456.rds"))
})

test_that("a survey id that is not a number is rejected before any request", {
  calls <- survey_api(1)
  expect_error(
    fetch_survey("../x", dir = withr::local_tempdir()),
    "survey number"
  )
  expect_error(
    fetch_survey("1/surveyresponse", dir = withr::local_tempdir()),
    "survey number"
  )
  expect_error(fetch_data_dictionary("1/surveyresponse"), "survey number")
  expect_error(fetch_survey(c(1, 2)), "one survey number")
  expect_length(calls$reqs, 0)
})

test_that("refresh = 'local' makes no request and needs a file", {
  dir <- withr::local_tempdir()
  calls <- survey_api(2)
  expect_error(fetch_survey(123, dir = dir, refresh = "local"), "No local file")
  suppressMessages(fetch_survey(123, dir = dir))
  n <- length(calls$reqs)
  expect_message(
    out <- fetch_survey(123, dir = dir, refresh = "local"),
    "0 requests"
  )
  expect_equal(nrow(out), 2)
  expect_length(calls$reqs, n)
})

test_that("min_age decides between the file and a new pull", {
  dir <- withr::local_tempdir()
  calls <- survey_api(2)
  suppressMessages(fetch_survey(123, dir = dir))
  file <- file.path(dir, "survey_123.rds")
  n <- length(calls$reqs)

  expect_message(
    fetch_survey(123, dir = dir, min_age = 600),
    "local file \\(checked 0 min ago\\), 0 requests"
  )
  expect_length(calls$reqs, n)

  Sys.setFileTime(file, Sys.time() - 3600)
  suppressMessages(fetch_survey(123, dir = dir, min_age = 600))
  expect_gt(length(calls$reqs), n)

  n <- length(calls$reqs)
  Sys.setFileTime(file, Sys.time() + 3600) # a clock in the future counts as stale
  suppressMessages(fetch_survey(123, dir = dir, min_age = 600))
  expect_gt(length(calls$reqs), n)
})

test_that("test data is filtered by the API unless include_test = TRUE", {
  dir <- withr::local_tempdir()
  calls <- survey_api(2)
  suppressMessages(fetch_survey(123, dir = dir))
  q <- query_of(calls$reqs[[length(calls$reqs)]])
  expect_equal(
    c(q$`filter[field][0]`, q$`filter[value][0]`),
    c("is_test_data", "0")
  )

  suppressMessages(fetch_survey(123, dir = dir, include_test = TRUE))
  q <- query_of(calls$reqs[[length(calls$reqs)]])
  expect_null(q$`filter[field][0]`)
})

test_that("a survey without responses gives a zero-row tibble with all columns", {
  dir <- withr::local_tempdir()
  survey_api(0)
  out <- suppressMessages(fetch_survey(123, dir = dir))
  expect_equal(nrow(out), 0)
  expect_true(all(c("survey_id", "Q3", "Q6", "fetched_at") %in% names(out)))
  expect_type(out$Q16, "double")
})

test_that("an unwrapped single object and a team array do not break parsing", {
  mock_api(\(req) json(list(result_ok = TRUE, id = 5, team = list(1, 2))))
  body <- api_get("survey/5")
  expect_equal(body$id, 5)
  expect_equal(unlist(body$team), c(1, 2))
})

test_that("a round numeric survey id is not turned into scientific notation", {
  calls <- survey_api(1)
  expect_error(fetch_survey(50000000, dir = withr::local_tempdir()))
  expect_match(calls$reqs[[1]]$url, "/survey/50000000\\?")
})

test_that("a folder that does not exist yet is created for the file", {
  dir <- file.path(withr::local_tempdir(), "data", "raw")
  survey_api(2)
  out <- suppressMessages(fetch_survey(123, dir = dir))
  expect_equal(nrow(out), 2)
  expect_true(file.exists(file.path(dir, "survey_123.rds")))
})
