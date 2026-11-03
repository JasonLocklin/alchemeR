# All network code lives here. Every request is a read-only GET (ADR-4, #35).

api_base <- function() {
  Sys.getenv("ALCHEMER_BASE", "https://api.alchemer-ca.com/v5")
}

# Seconds to wait before retry number `i` (429 and 5xx); both mocked in tests.
api_backoff <- function(i) min(2^i, 30)

# Pause before the one restart at smaller pages.
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

# GET one path and return the parsed JSON body. Errors are one line and never
# contain the credentials. A page that still fails with a 5xx or a network
# error after the retries is signalled as class "alchemer_timeout" so the
# pager can fall back to smaller pages.
api_get <- function(
  path,
  query = list(),
  token = Sys.getenv("ALCHEMER_API_TOKEN"),
  secret_key = Sys.getenv("ALCHEMER_API_SECRET"),
  simplify = TRUE
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

  req <- api_request(path, query, token, secret_key)

  resp <- tryCatch(
    httr2::req_perform(req),
    error = \(e) fail(conditionMessage(e), "alchemer_timeout")
  )

  status <- httr2::resp_status(resp)
  body <- tryCatch(
    jsonlite::fromJSON(
      httr2::resp_body_string(resp),
      simplifyVector = simplify,
      flatten = simplify
    ),
    error = \(e) NULL
  )
  if (status >= 500) {
    fail(paste("HTTP", status), "alchemer_timeout")
  }
  if (status >= 400 || is.null(body) || isFALSE(body$result_ok)) {
    msg <- if (is.list(body) && !is.null(body$message)) body$message else ""
    fail(paste0("HTTP ", status, " ", paste(msg, collapse = " ")))
  }
  body
}

# GET every page of a list endpoint and return the page bodies. Checks that
# the rows received equal `total_count`; never stops at a page cap. If a page
# times out after the retries, wait and restart the whole pull once at 100 rows
# per page.
api_get_all <- function(path, query = list(), per_page = 500, ...) {
  pull <- function(n) {
    get_page <- \(p) {
      api_get(path, c(query, list(resultsperpage = n, page = p)), ...)
    }
    first <- get_page(1)
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
    pages <- c(
      list(first),
      lapply(seq_len(max(total_pages - 1, 0)) + 1, get_page)
    )
    rows <- sum(vapply(pages, \(x) NROW(x$data), numeric(1)))
    if (rows != total_count) {
      stop(
        "GET ",
        path,
        ": received ",
        rows,
        " rows but total_count is ",
        total_count,
        call. = FALSE
      )
    }
    pages
  }
  tryCatch(
    pull(per_page),
    alchemer_timeout = function(e) {
      if (per_page <= 100) {
        stop(e)
      }
      message(
        "A page timed out; waiting 60 s, then retrying at 100 rows per page."
      )
      api_wait()
      pull(100)
    }
  )
}
