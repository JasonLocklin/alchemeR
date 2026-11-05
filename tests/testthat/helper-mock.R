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

# A fake account with one survey (123) whose responses live in `acct$rows` (a
# named list, id -> response). It answers the survey list, the definition and
# response requests with paging, `order_by` and the filters the package uses
# (date_updated >= / <, is_test_data =), so tests can change the account
# between calls: add, edit or delete rows, or change `acct$modified_on`.
fake_account <- function(n = 0, env = parent.frame()) {
  acct <- new.env()
  acct$modified_on <- "2026-10-01 10:00:00"
  acct$rows <- list()
  acct$next_id <- 1
  acct$add <- function(k = 1, minutes = NULL, ...) {
    for (i in seq_len(k)) {
      id <- acct$next_id
      acct$next_id <- id + 1
      upd <- format(
        as.POSIXct("2026-11-01 05:00:00", tz = "UTC") +
          60 * (if (is.null(minutes)) id else minutes),
        "%Y-%m-%d %H:%M:%S"
      )
      acct$rows[[as.character(id)]] <- response(
        id,
        full_answers(),
        date_updated = upd,
        ...
      )
    }
  }
  acct$add(n)
  calls <- mock_api(
    function(req) {
      path <- httr2::url_parse(req$url)$path
      q <- query_of(req)
      if (grepl("surveyresponse$", path)) {
        rows <- unname(acct$rows)
        i <- 0
        while (!is.null(q[[paste0("filter[field][", i, "]")]])) {
          f <- q[[paste0("filter[field][", i, "]")]]
          op <- q[[paste0("filter[operator][", i, "]")]]
          v <- q[[paste0("filter[value][", i, "]")]]
          rows <- Filter(
            \(r) {
              switch(
                op,
                ">=" = r[[f]] >= v,
                "<" = r[[f]] < v,
                "=" = r[[f]] == v
              )
            },
            rows
          )
          i <- i + 1
        }
        if (!is.null(q$order_by)) {
          rows <- rows[order(
            vapply(rows, \(r) r$date_updated, ""),
            decreasing = TRUE
          )]
        }
        page_response(req, length(rows), \(i) rows[[i]])
      } else if (grepl("survey/123$", path)) {
        json(list(
          result_ok = TRUE,
          data = c(
            utils::modifyList(
              definition(),
              list(modified_on = acct$modified_on)
            ),
            title = "Demo",
            status = "Launched"
          )
        ))
      } else {
        json(list(
          result_ok = TRUE,
          total_count = 1,
          total_pages = 1,
          page = 1,
          data = list(list(
            id = 123,
            title = "Demo",
            status = "Launched",
            type = "survey",
            team = list(1, 2),
            created_on = "2026-09-01 10:00:00",
            modified_on = acct$modified_on
          ))
        ))
      }
    },
    env = env
  )
  calls$acct <- acct
  calls
}

# Shorthand for the common case: `n` responses, ids 1..n (or `ids`).
survey_api <- function(n, ids = seq_len(n), env = parent.frame()) {
  calls <- fake_account(0, env)
  for (id in ids) {
    calls$acct$next_id <- id
    calls$acct$add(1, minutes = id)
  }
  calls
}
