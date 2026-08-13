test_that("all_surveys warns once and delegates to alchemer_surveys", {
  httr2::local_mocked_responses(list(fixture_response("survey_list.json")))
  rlang::local_options(lifecycle_verbosity = "warning")
  expect_warning(out <- all_surveys("TOKEN", "SECRET"), class = "lifecycle_warning_deprecated")
  expect_equal(nrow(out), 2)
})

test_that("fetch_survey no longer writes a CSV unless file= is supplied", {
  httr2::local_mocked_responses(list(fixture_response("surveyresponse_list.json")))
  rlang::local_options(lifecycle_verbosity = "quiet")
  dir <- withr::local_tempdir()
  withr::local_dir(dir)
  out <- fetch_survey("8611799", "TOKEN", "SECRET")
  expect_equal(nrow(out), 2)
  expect_length(list.files(dir, pattern = "\\.csv$"), 0)
})

test_that("fetch_survey writes a CSV when file= is supplied explicitly", {
  httr2::local_mocked_responses(list(fixture_response("surveyresponse_list.json")))
  rlang::local_options(lifecycle_verbosity = "quiet")
  path <- withr::local_tempfile(fileext = ".csv")
  fetch_survey("8611799", "TOKEN", "SECRET", file = path)
  expect_true(file.exists(path))
  expect_equal(nrow(utils::read.csv(path)), 2)
})

test_that("deprecated shims fall back to environment variables when called with no credentials", {
  # Regression test (ultrareview): token/secret_key used to default to the
  # literal placeholder strings "token"/"secret_key" rather than NULL, which
  # bypassed alchemer_creds()'s Sys.getenv() fallback entirely and sent the
  # placeholder text to the API as real credentials instead of the ones the
  # user actually configured.
  withr::local_envvar(ALCHEMER_API_TOKEN = "env-token", ALCHEMER_API_SECRET = "env-secret")
  rlang::local_options(lifecycle_verbosity = "quiet")
  sent_url <- NULL
  capture_url <- function(req) {
    sent_url <<- req$url
    fixture_response("surveyresponse_list.json")
  }

  httr2::local_mocked_responses(capture_url)
  fetch_survey("8611799")
  expect_true(grepl("api_token=env-token", sent_url, fixed = TRUE))
  expect_true(grepl("api_token_secret=env-secret", sent_url, fixed = TRUE))
  expect_false(grepl("api_token=token", sent_url, fixed = TRUE))

  httr2::local_mocked_responses(capture_url)
  fetch_data_dictionary("8611799")
  expect_true(grepl("api_token=env-token", sent_url, fixed = TRUE))
  expect_false(grepl("api_token=token", sent_url, fixed = TRUE))

  httr2::local_mocked_responses(capture_url)
  all_surveys()
  expect_true(grepl("api_token=env-token", sent_url, fixed = TRUE))
})

test_that("fetch_data_dictionary warns and delegates to alchemer_questions", {
  httr2::local_mocked_responses(list(fixture_response("surveyquestion_list.json")))
  rlang::local_options(lifecycle_verbosity = "warning")
  expect_warning(
    out <- fetch_data_dictionary("8611799", "TOKEN", "SECRET"),
    class = "lifecycle_warning_deprecated"
  )
  expect_equal(nrow(out), 3)
})

test_that("deprecated shims' output shapes are documented accurately (ultrareview follow-up)", {
  # These functions delegate to their replacements and were never intended
  # to reproduce the pre-1.0 output shape byte-for-byte; ultrareview flagged
  # that the roxygen docs/NEWS.md didn't say so clearly enough. These tests
  # lock in and document the actual current shapes, so a future accidental
  # shape change (as opposed to the deliberate one this package already
  # made) is caught here rather than only discovered downstream.
  rlang::local_options(lifecycle_verbosity = "quiet")

  # all_surveys(): nested fields are list-columns, not flattened dotted names
  httr2::local_mocked_responses(list(fixture_response("survey_list.json")))
  surveys <- all_surveys("TOKEN", "SECRET")
  expect_true(is.list(surveys$statistics))
  expect_false("statistics.Complete" %in% names(surveys))

  # fetch_data_dictionary(): alchemer_questions()'s columns, not the old
  # question_id/label/options_id/options_value shape
  httr2::local_mocked_responses(list(fixture_response("surveyquestion_list.json")))
  dict <- fetch_data_dictionary("8611799", "TOKEN", "SECRET")
  expect_true(all(c("id", "title", "shortname", "varname") %in% names(dict)))
  expect_false(any(c("label", "options_id", "options_value") %in% names(dict)))

  # fetch_survey(file=): one survey_data JSON column, not one Q<id> column
  # per question
  httr2::local_mocked_responses(list(fixture_response("surveyresponse_list.json")))
  path <- withr::local_tempfile(fileext = ".csv")
  fetch_survey("8611799", "TOKEN", "SECRET", file = path)
  csv_cols <- names(utils::read.csv(path, nrows = 0))
  expect_true("survey_data" %in% csv_cols)
  expect_false(any(grepl("^Q[0-9]+$", csv_cols)))
})
