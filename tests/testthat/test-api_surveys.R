test_that("alchemer_surveys returns a tibble, one row per survey", {
  httr2::local_mocked_responses(list(fixture_response("survey_list.json")))
  out <- alchemer_surveys(test_client())
  expect_s3_class(out, "tbl_df")
  expect_equal(nrow(out), 2)
  expect_equal(out$id, c("8611799", "8611800"))
  expect_true(is.list(out$statistics))
})

test_that("alchemer_survey returns one row holding the full definition tree", {
  httr2::local_mocked_responses(list(fixture_response("survey_get.json")))
  out <- alchemer_survey("8611799", test_client())
  expect_equal(nrow(out), 1)
  expect_equal(out$id, "8611799")
  pages <- out$pages[[1]]
  expect_equal(length(pages), 1)
  expect_equal(length(pages[[1]]$questions), 3)
})

test_that("alchemer_questions returns a tibble including varname", {
  httr2::local_mocked_responses(list(fixture_response("surveyquestion_list.json")))
  out <- alchemer_questions("8611799", test_client())
  expect_equal(nrow(out), 3)
  expect_equal(out$varname[out$id == "2"], "first_name")
})

test_that("alchemer_responses returns one row per response, including edge cases", {
  httr2::local_mocked_responses(list(fixture_response("surveyresponse_list.json")))
  out <- alchemer_responses("8611799", test_client())
  expect_equal(nrow(out), 2)
  # response 121: url_variables [] and data_quality [] (clean/empty cases)
  expect_equal(out$is_test_data, c("0", "1"))
  # survey_data is preserved as a list-column, verbatim, for pub_layer() to explode later
  expect_true(is.list(out$survey_data[[1]]))
  expect_null(out$survey_data[[1]][["82"]]$answer)
  expect_false(out$survey_data[[1]][["82"]]$shown)
})

test_that("alchemer_responses passes extra query parameters through, e.g. order_by", {
  httptest2::without_internet({
    req <- alchemer_request(
      test_client(), "survey/8611799/surveyresponse",
      query = list(order_by = "-date_updated")
    )
    httptest2::expect_GET(
      httr2::req_perform(req),
      paste0(
        "https://api.alchemer.com/v5/survey/8611799/surveyresponse?",
        "api_token=TOKEN&api_token_secret=SECRET&order_by=-date_updated"
      )
    )
  })
})

test_that("alchemer_campaigns returns a tibble", {
  httr2::local_mocked_responses(list(fixture_response("surveycampaign_list.json")))
  out <- alchemer_campaigns("8611799", test_client())
  expect_equal(nrow(out), 1)
  expect_equal(out$name, "Default Link")
})

test_that("alchemer_statistics survives the 'total responses' space-in-key gotcha", {
  httr2::local_mocked_responses(list(fixture_response("surveystatistic_list.json")))
  out <- alchemer_statistics("8611799", test_client())
  expect_equal(nrow(out), 3)
  expect_true("total responses" %in% names(out))
  expect_equal(out[["total responses"]][out$id == 26], "3")
})
