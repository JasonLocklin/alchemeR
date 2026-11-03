test_that("api_get returns the parsed body and sends a GET with credentials", {
  calls <- mock_api(\(req) json(list(result_ok = TRUE, data = list(id = 1))))
  body <- api_get("survey/1")
  expect_equal(body$data$id, 1)
  req <- calls$reqs[[1]]
  expect_equal(req$method, "GET")
  expect_equal(query_of(req)$api_token, "TOK123")
})

test_that("missing credentials fail before any request", {
  calls <- mock_api(\(req) json(list(result_ok = TRUE)))
  withr::local_envvar(ALCHEMER_API_TOKEN = "", ALCHEMER_API_SECRET = "")
  expect_error(api_get("survey"), "credentials are missing")
  expect_length(calls$reqs, 0)
})

test_that("the request is a throttled GET that retries 429 and 5xx", {
  # httr2 mocks bypass retry and throttle, so check the policies themselves.
  req <- api_request("survey", list(page = 1), "t", "s")
  expect_equal(req$method, "GET")
  expect_equal(req$policies$retry_max_tries, 5)
  expect_true(!is.null(req$policies$throttle_realm))
  transient <- req$policies$retry_is_transient
  expect_true(all(vapply(
    c(429, 500, 502, 503, 504),
    \(s) transient(json(list(), s)),
    TRUE
  )))
  expect_false(transient(json(list(), 401)))

  # 429: wait 30 s (or Retry-After) and say so; 5xx: the short backoff
  after <- req$policies$retry_after
  expect_message(expect_equal(after(json(list(), 429)), 30), "rate limit")
  expect_message(
    expect_equal(
      after(httr2::response(429, headers = list(`Retry-After` = "7"))),
      7
    ),
    "waiting 7 s"
  )
  expect_true(is.na(after(json(list(), 500))))
})

test_that("error bodies give a one-line error without credentials or URL", {
  mock_api(\(req) {
    json(list(result_ok = FALSE, code = 401, message = "Bad key TOK123"), 401)
  })
  err <- expect_error(api_get("survey/1"), "HTTP 401")
  expect_false(grepl("TOK123|SEC456|api_token", conditionMessage(err)))
  expect_length(strsplit(conditionMessage(err), "\n")[[1]], 1)
})

test_that("result_ok = FALSE with status 200 is an error", {
  mock_api(\(req) json(list(result_ok = FALSE, message = "nope")))
  expect_error(api_get("survey"), "nope")
})

test_that("transport errors never show the token", {
  mock_api(\(req) {
    stop(
      "Could not connect to https://x/?api_token=TOK123&api_token_secret=SEC456"
    )
  })
  err <- expect_error(api_get("survey"), "GET survey")
  expect_false(grepl("TOK123|SEC456", conditionMessage(err)))
  expect_s3_class(err, "alchemer_timeout")
})

test_that("api_get_all collects every page (more than 250 of them)", {
  # 300 pages of one row each, more than the old 250-page cap.
  calls <- mock_api(\(req) {
    p <- as.integer(query_of(req)$page)
    json(list(
      result_ok = TRUE,
      total_count = 300,
      total_pages = 300,
      page = p,
      data = list(list(id = p))
    ))
  })
  pages <- api_get_all("survey/1/surveyresponse")
  expect_length(pages, 300)
  expect_equal(pages[[300]]$data$id, 300)
})

test_that("api_get_all errors when rows differ from total_count", {
  mock_api(\(req) {
    json(list(
      result_ok = TRUE,
      total_count = 5,
      total_pages = 1,
      page = 1,
      data = list(list(id = 1))
    ))
  })
  expect_error(api_get_all("survey/1/surveyresponse"), "received 1 rows")
})

test_that("a timeout restarts the whole pull once at 100 per page", {
  calls <- mock_api(\(req) {
    if (query_of(req)$resultsperpage == "500") {
      json(list(result_ok = FALSE, message = "timeout"), 500)
    } else {
      page_response(req, 250, \(i) list(id = i))
    }
  })
  expect_message(
    pages <- api_get_all("survey/1/surveyresponse"),
    "100 rows per page"
  )
  expect_length(pages, 3)
  per <- vapply(calls$reqs, \(r) query_of(r)$resultsperpage, "")
  expect_equal(unique(per), c("500", "100"))
})

test_that("a timeout at 100 per page is an error", {
  mock_api(\(req) json(list(result_ok = FALSE), 500))
  expect_error(
    suppressMessages(api_get_all("survey/1/surveyresponse")),
    "HTTP 500"
  )
})

test_that("every request of every exported function is a GET without _method", {
  calls <- survey_api(5)
  all_surveys()
  fetch_data_dictionary(123)
  suppressMessages(fetch_survey(123, dir = withr::local_tempdir()))
  expect_gte(length(calls$reqs), 4)
  for (req in calls$reqs) {
    expect_equal(req$method, "GET")
    expect_null(req$body)
    expect_false("_method" %in% names(query_of(req)))
  }
})
