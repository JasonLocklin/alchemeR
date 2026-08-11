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

test_that("fetch_data_dictionary warns and delegates to alchemer_questions", {
  httr2::local_mocked_responses(list(fixture_response("surveyquestion_list.json")))
  rlang::local_options(lifecycle_verbosity = "warning")
  expect_warning(
    out <- fetch_data_dictionary("8611799", "TOKEN", "SECRET"),
    class = "lifecycle_warning_deprecated"
  )
  expect_equal(nrow(out), 3)
})
