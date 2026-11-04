# All network code lives here. Every request is a read-only GET (ADR-4, #35).

api_base <- function() {
  Sys.getenv("ALCHEMER_BASE", "https://api.alchemer-ca.com/v5")
}

# Seconds to wait before retry number `i` (429 and 5xx); both mocked in tests.
api_backoff <- function(i) min(2^i, 30)

# Requests sent in this R session; fetch_survey() reports the difference.
api_stats <- new.env()
api_stats$requests <- 0

# Pause before a restart of a pull: lets Alchemer's 60 s GET cache expire.
api_wait <- function() Sys.sleep(60)

api_is_transient <- function(resp) {
  httr2::resp_status(resp) %in% c(429, 500, 502, 503, 504)
}

# Seconds to wait after a 429: the account limit (240/min) is shared with
# colleagues and counted over a minute, so wait for the Retry-After header or
# 30 s. NA (other statuses) uses the short backoff.
api_retry_after <- function(resp) {
  if (httr2::resp_status(resp) != 429) {
    return(NA_real_)
  }
  wait <- httr2::resp_retry_after(resp)
  if (is.na(wait)) {
    wait <- 30
  }
  message("Alchemer rate limit reached; waiting ", wait, " s.")
  wait
}

# The request: GET only, at most 2 per second (half the 240/min account limit,
# which colleagues share), 429/5xx retried.
api_request <- function(path, query, token, secret_key) {
  httr2::request(api_base()) |>
    httr2::req_url_path_append(path) |>
    httr2::req_url_query(
      !!!query,
      api_token = token,
      api_token_secret = secret_key
    ) |>
    httr2::req_method("GET") |>
    httr2::req_timeout(120) |>
    httr2::req_throttle(rate = 2) |>
    httr2::req_retry(
      max_tries = 5,
      is_transient = api_is_transient,
      after = api_retry_after,
      backoff = \(i) api_backoff(i)
    ) |>
    httr2::req_error(is_error = \(resp) FALSE)
}

# GET one path and return the parsed JSON body (lists, as in the JSON).
api_get <- function(
  path,
  query = list(),
  token = Sys.getenv("ALCHEMER_API_TOKEN"),
  secret_key = Sys.getenv("ALCHEMER_API_SECRET")
) {
  api_get_many(path, list(query), token, secret_key)[[1]]
}

# Pages requested at once by api_get_many(), and rows per page. Full
# download of 8 805 rows (probe_final F3, probe_pagesize, 2026-11-04): 500
# rows one at a time 220 s; 6 at once: 100 rows 55-65 s (87 requests), 250
# rows 55 s (36 requests), 500 rows 64 s (18 requests).
api_parallel <- 6
api_per_page <- 250

# GET one path with several queries (pages), up to `api_parallel` at a time,
# and return the parsed bodies in order. Errors are one line and never contain
# the credentials. A 429/5xx or network error is retried (api_request); one
# that still fails after the retries is an error.
api_get_many <- function(
  path,
  queries,
  token = Sys.getenv("ALCHEMER_API_TOKEN"),
  secret_key = Sys.getenv("ALCHEMER_API_SECRET")
) {
  if (!nzchar(token) || !nzchar(secret_key)) {
    stop(
      "Alchemer credentials are missing: set ALCHEMER_API_TOKEN and ",
      "ALCHEMER_API_SECRET (e.g. in .Renviron) or pass token and secret_key.",
      call. = FALSE
    )
  }
  redact <- function(x) {
    x <- gsub(token, "<token>", x, fixed = TRUE)
    gsub(secret_key, "<secret>", x, fixed = TRUE)
  }
  fail <- function(msg, class = "alchemer_error") {
    stop(
      structure(
        class = c(class, "error", "condition"),
        list(message = paste0("GET ", path, ": ", redact(msg)), call = NULL)
      )
    )
  }

  reqs <- lapply(queries, \(q) api_request(path, q, token, secret_key))
  api_stats$requests <- api_stats$requests + length(reqs)
  resps <- tryCatch(
    httr2::req_perform_parallel(
      reqs,
      on_error = "continue",
      progress = FALSE,
      max_active = api_parallel
    ),
    error = \(e) fail(conditionMessage(e))
  )

  lapply(resps, \(resp) {
    if (inherits(resp, "error")) {
      fail(conditionMessage(resp))
    }
    status <- httr2::resp_status(resp)
    body <- tryCatch(
      jsonlite::fromJSON(httr2::resp_body_string(resp), simplifyVector = FALSE),
      error = \(e) NULL
    )
    if (status >= 500) {
      fail(paste("HTTP", status))
    }
    if (status >= 400 || is.null(body) || isFALSE(body$result_ok)) {
      msg <- if (is.list(body) && !is.null(body$message)) body$message else ""
      fail(paste0("HTTP ", status, " ", paste(msg, collapse = " ")))
    }
    body
  })
}

# GET every page of a list endpoint and return the page bodies. Checks that
# the rows received equal `total_count`; never stops at a page cap.
# - Rows differ from `total_count` (a response was submitted or deleted while
#   the pages were read): wait, then restart the pull once; error if it
#   differs again.
# A page that fails after its retries is an error (250-row pages; no smaller
# fallback since 2026-11-04).
api_get_all <- function(path, query = list(), per_page = api_per_page, ...) {
  pull <- function() {
    page_query <- \(p) c(query, list(resultsperpage = per_page, page = p))
    first <- api_get(path, page_query(1), ...)
    total_pages <- as.integer(first$total_pages)
    total_count <- as.numeric(first$total_count)
    if (is.na(total_pages) || is.na(total_count)) {
      stop(
        "GET ",
        path,
        ": response has no total_pages/total_count",
        call. = FALSE
      )
    }
    # The other pages are requested a few at a time (api_get_many).
    rest <- seq_len(max(total_pages - 1, 0)) + 1
    pages <- c(list(first), api_get_many(path, lapply(rest, page_query), ...))
    rows <- sum(vapply(pages, \(x) NROW(x$data), numeric(1)))
    if (rows != total_count) {
      msg <- paste0(
        "GET ",
        path,
        ": received ",
        rows,
        " rows but total_count is ",
        total_count
      )
      stop(errorCondition(msg, class = "alchemer_count", call = NULL))
    }
    pages
  }
  # Alchemer answers an identical GET within 60 s from its cache, so the
  # restart waits first; otherwise it would get the same stale pages back.
  tryCatch(pull(), alchemer_count = function(e) {
    message(
      conditionMessage(e),
      " (responses changed during the download); waiting 60 s, ",
      "then downloading again."
    )
    api_wait()
    pull()
  })
}
