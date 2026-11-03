# Mock the Alchemer API: `handler(req)` returns an httr2 response. Every
# request is recorded in `calls` (a list in an environment).
mock_api <- function(handler, env = parent.frame()) {
  calls <- new.env()
  calls$reqs <- list()
  withr::local_envvar(
    ALCHEMER_API_TOKEN = "TOK123",
    ALCHEMER_API_SECRET = "SEC456",
    .local_envir = env
  )
  testthat::local_mocked_bindings(
    api_backoff = \(i) 0,
    api_wait = \() NULL,
    .package = "alchemeR",
    .env = env
  )
  httr2::local_mocked_responses(
    function(req) {
      calls$reqs <- c(calls$reqs, list(req))
      handler(req)
    },
    env = env
  )
  calls
}

json <- function(body, status = 200) {
  httr2::response_json(status_code = status, body = body)
}

# A list response of `n` rows made of `row(i)`, paged like Alchemer does.
page_response <- function(req, n, row, total_pages = NULL) {
  q <- httr2::url_parse(req$url)$query
  per <- as.integer(q$resultsperpage)
  page <- as.integer(q$page)
  idx <- seq_len(n)
  idx <- idx[idx > (page - 1) * per & idx <= page * per]
  json(list(
    result_ok = TRUE,
    total_count = n,
    page = page,
    total_pages = max(ceiling(n / per), 1),
    results_per_page = as.character(per),
    data = lapply(idx, row)
  ))
}

query_of <- function(req) httr2::url_parse(req$url)$query
