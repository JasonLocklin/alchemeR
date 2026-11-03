test_that("all_surveys returns the survey table and errors above one page", {
  mock_api(\(req) {
    page_response(req, 3, \(i) list(id = i, title = paste("S", i)))
  })
  out <- all_surveys()
  expect_equal(nrow(out), 3)
  expect_equal(out$title, c("S 1", "S 2", "S 3"))

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

question <- function(i) {
  list(
    id = i,
    title = list(English = paste("Question", i)),
    options = list(list(id = 10001, value = "a"), list(id = 10002, value = "b"))
  )
}

test_that("fetch_data_dictionary returns all questions, not just 25", {
  calls <- mock_api(\(req) page_response(req, 60, question))
  dict <- fetch_data_dictionary(99)
  expect_named(dict, c("question_id", "label", "options_id", "options_value"))
  expect_equal(nrow(dict), 120)
  expect_equal(dict$question_id[1], glue::glue("Q1"))
  expect_equal(query_of(calls$reqs[[1]])$resultsperpage, "500")
})

response <- function(i) {
  list(
    id = i,
    status = "Complete",
    is_test_data = 0,
    date_submitted = "2025-01-01 10:00:00 GMT",
    response_time = 5,
    survey_data = list(`1` = list(id = 1, answer = paste("a", i)))
  )
}

test_that("fetch_survey writes every row to <survey_name>.csv", {
  dir <- withr::local_tempdir()
  withr::local_dir(dir)
  mock_api(\(req) page_response(req, 600, response))
  fetch_survey(99, survey_name = "s")
  out <- utils::read.csv("s.csv")
  expect_equal(nrow(out), 600)
  expect_named(
    out,
    c("id", "status", "is_test_data", "date_submitted", "response_time", "Q1")
  )
})

test_that("a failed page leaves the existing CSV untouched", {
  dir <- withr::local_tempdir()
  withr::local_dir(dir)
  writeLines("keep,me", "s.csv")
  mock_api(\(req) {
    if (query_of(req)$page == "2") {
      json(list(result_ok = FALSE), 400)
    } else {
      page_response(req, 600, response)
    }
  })
  expect_error(fetch_survey(99, survey_name = "s"), "HTTP 400")
  expect_equal(readLines("s.csv"), "keep,me")
})

test_that("a survey without responses is an error, not a broken CSV", {
  withr::local_dir(withr::local_tempdir())
  mock_api(\(req) page_response(req, 0, response))
  expect_error(fetch_survey(99), "no responses")
  expect_false(file.exists("survey_data.csv"))
})

test_that("team arrays and an unwrapped single object do not break parsing", {
  mock_api(\(req) json(list(result_ok = TRUE, id = 5, team = list(1, 2))))
  body <- api_get("survey/5")
  expect_equal(body$id, 5)
  expect_equal(unlist(body$team), c(1, 2))
})
