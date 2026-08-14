# test_client() is defined in helper-fixtures.R

# --- URL construction (httptest2) -------------------------------------------

test_that("alchemer_request builds the credentialed URL against the right domain", {
  client <- test_client()
  httptest2::without_internet({
    req <- alchemer_request(client, "survey/123/surveyresponse", query = list(page = 2))
    httptest2::expect_GET(
      httr2::req_perform(req),
      "https://api.alchemer.com/v5/survey/123/surveyresponse?api_token=TOKEN&api_token_secret=SECRET&page=2"
    )
  })
})

test_that("alchemer_client respects ALCHEMER_DOMAIN for regional accounts", {
  client <- alchemer_client(token = "T", secret = "S", domain = "api.alchemer.eu")
  httptest2::without_internet({
    req <- alchemer_request(client, "survey")
    httptest2::expect_GET(
      httr2::req_perform(req),
      "https://api.alchemer.eu/v5/survey?api_token=T&api_token_secret=S"
    )
  })
})

# --- redaction ---------------------------------------------------------------

test_that("redact_url strips credentials and pagination, keeping only path", {
  url <- "https://api.alchemer.com/v5/survey/123/surveyresponse?api_token=T&api_token_secret=S&page=2"
  expect_equal(redact_url(url), "https://api.alchemer.com/v5/survey/123/surveyresponse")
  expect_false(grepl("api_token", redact_url(url)))
})

# --- retry classification ----------------------------------------------------

test_that("is_transient_status flags exactly the documented retryable statuses", {
  transient <- c(429, 500, 502, 503, 504)
  permanent <- c(200, 400, 401, 403, 404)
  for (s in transient) {
    expect_true(is_transient_status(httr2::response(status_code = s)), info = as.character(s))
  }
  for (s in permanent) {
    expect_false(is_transient_status(httr2::response(status_code = s)), info = as.character(s))
  }
})

# --- envelope variants --------------------------------------------------------

test_that("unwrap_envelope unwraps a data-wrapped array (the common list-endpoint shape)", {
  req <- httr2::request("https://api.alchemer.com/v5/survey")
  body <- list(result_ok = TRUE, total_count = 1, data = list(list(id = "1", title = "A")))
  expect_equal(unwrap_envelope(body, req), list(list(id = "1", title = "A")))
})

test_that("unwrap_envelope unwraps a data-wrapped map (theme/customfield shape)", {
  req <- httr2::request("https://api.alchemer.com/v5/surveytheme")
  body <- list(result_ok = TRUE, data = list(`68962` = list(id = 68962, name = "Theme")))
  expect_equal(unwrap_envelope(body, req), list(`68962` = list(id = 68962, name = "Theme")))
})

test_that("unwrap_envelope passes through a top-level (unwrapped) single-object GET", {
  req <- httr2::request("https://api.alchemer.com/v5/survey/1/surveypage/1")
  body <- list(result_ok = TRUE, id = "1", title = "Page 1", description = "")
  expect_equal(unwrap_envelope(body, req), list(id = "1", title = "Page 1", description = ""))
})

test_that("unwrap_envelope treats a missing result_ok as success (surveypage-get has none)", {
  req <- httr2::request("https://api.alchemer.com/v5/survey/1/surveypage/1")
  body <- list(id = "1", title = "Page 1")
  expect_equal(unwrap_envelope(body, req), list(id = "1", title = "Page 1"))
})

test_that("unwrap_envelope raises a typed error on result_ok: false", {
  req <- httr2::request("https://api.alchemer.com/v5/survey")
  body <- list(result_ok = FALSE, code = 401, message = "Login failed / Invalid auth token")
  err <- rlang::catch_cnd(unwrap_envelope(body, req), classes = "alchemeR_api_result_error")
  expect_s3_class(err, "alchemeR_api_result_error")
  expect_equal(err$code, 401)
})

test_that("unwrap_envelope's error never embeds the credentialed URL", {
  req <- httr2::request("https://api.alchemer.com/v5/survey?api_token=SECRET&api_token_secret=SECRET2")
  body <- list(result_ok = FALSE, code = 401, message = "Login failed")
  err <- rlang::catch_cnd(unwrap_envelope(body, req), classes = "alchemeR_api_result_error")
  expect_false(grepl("SECRET", conditionMessage(err)))
})

# --- pagination termination --------------------------------------------------

mock_page <- function(page, total_pages, n, start_id = 1) {
  httr2::response_json(
    status_code = 200,
    body = list(
      result_ok = TRUE, page = page, total_pages = total_pages, results_per_page = n,
      data = purrr::map(seq(start_id, length.out = n), ~ list(id = as.character(.x)))
    )
  )
}

test_that("alchemer_fetch_all stops using total_pages/page and combines every page", {
  httr2::local_mocked_responses(list(
    mock_page(1, 3, 2, start_id = 1),
    mock_page(2, 3, 2, start_id = 3),
    mock_page(3, 3, 1, start_id = 5)
  ))
  out <- alchemer_fetch_all(test_client(), "survey")
  expect_length(out, 5)
  expect_equal(purrr::map_chr(out, "id"), as.character(1:5))
})

test_that("alchemer_fetch_all stops on an empty page when total_pages/page are absent", {
  empty_page <- httr2::response_json(status_code = 200, body = list(result_ok = TRUE, data = list()))
  httr2::local_mocked_responses(list(
    mock_page(1, NULL, 2, start_id = 1),
    empty_page
  ))
  out <- alchemer_fetch_all(test_client(), "contactlist")
  expect_length(out, 2)
})

test_that("a caller-supplied resultsperpage survives instead of being overwritten", {
  # alchemer_responses() documents passing `resultsperpage` through `...`, but
  # alchemer_fetch_all() unconditionally reset it to 500, so it silently did
  # nothing. It is the one knob that can get a very wide survey under
  # Alchemer's 30-second response timeout, so "documented but ignored" was the
  # worst possible state for it.
  seen <- NULL
  httr2::local_mocked_responses(function(req) {
    seen <<- httr2::url_parse(req$url)$query$resultsperpage
    httr2::response_json(status_code = 200, body = list(
      result_ok = TRUE, page = 1, total_pages = 1, data = list(list(id = "1"))
    ))
  })

  alchemer_fetch_all(test_client(), "survey", query = list(resultsperpage = 50))
  expect_equal(seen, "50")

  alchemer_fetch_all(test_client(), "survey")
  expect_equal(seen, "500")
})

test_that("alchemer_fetch_all raises a typed, code-bearing error on result_ok: false mid-pagination", {
  httr2::local_mocked_responses(list(
    mock_page(1, 2, 2, start_id = 1),
    httr2::response_json(status_code = 200, body = list(result_ok = FALSE, code = 500, message = "oops"))
  ))
  expect_error(alchemer_fetch_all(test_client(), "survey"), class = "alchemeR_api_result_error")
})

# --- alchemer_fetch (single resource) ----------------------------------------

test_that("alchemer_fetch returns the unwrapped payload for a single-object GET", {
  httr2::local_mocked_responses(list(
    httr2::response_json(status_code = 200, body = list(id = "1", title = "Page 1"))
  ))
  out <- alchemer_fetch(test_client(), "survey/1/surveypage/1")
  expect_equal(out, list(id = "1", title = "Page 1"))
})

test_that("or_default() substitutes on NA as well as NULL, unlike %||%", {
  # Regression test (ultrareview): status %||% 'unknown...' never fired
  # because status was NA_integer_ (from a tryCatch fallback on a
  # connection failure), not NULL, and base R's %||% only triggers on NULL.
  expect_equal(or_default(NA_integer_, "fallback"), "fallback")
  expect_equal(or_default(NA, "fallback"), "fallback")
  expect_equal(or_default(NULL, "fallback"), "fallback")
  expect_equal(or_default(404L, "fallback"), 404L)
  expect_equal(or_default(0L, "fallback"), 0L)
})

test_that("alchemer_fetch never leaks the credentialed URL in an HTTP-error message", {
  httr2::local_mocked_responses(list(httr2::response(status_code = 404)))
  err <- tryCatch(
    alchemer_fetch(test_client(), "survey/999999"),
    error = function(e) e
  )
  expect_s3_class(err, "alchemeR_api_error")
  expect_false(grepl("TOKEN|SECRET", conditionMessage(err)))
  expect_true(grepl("survey/999999", conditionMessage(err)))
})
