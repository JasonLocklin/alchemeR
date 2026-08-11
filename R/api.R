# All network access goes through the functions in this file (ADR-010,
# ADR-011). Nothing else in the package may call httr2::request() or
# jsonlite::fromJSON() on a URL directly, and no function here may write a
# full request URL (it carries api_token/api_token_secret) to a message,
# warning, or log — only method, path, and status are ever surfaced.

#' Build an Alchemer API client
#'
#' All network access is made through a client object, so that ingestion can
#' be tested against a fixture-backed client without credentials (ADR-010).
#'
#' @param token,secret Passed to [alchemer_creds()].
#' @param domain Passed to [alchemer_domain()].
#' @param rpm Passed to [alchemer_rpm()].
#' @return An `alchemer_client` object.
#' @export
alchemer_client <- function(token = NULL, secret = NULL, domain = NULL, rpm = NULL) {
  creds <- alchemer_creds(token, secret)
  domain <- alchemer_domain(domain)
  structure(
    list(
      token = creds$token,
      secret = creds$secret,
      base_url = paste0("https://", domain, "/v5"),
      rpm = alchemer_rpm(rpm)
    ),
    class = "alchemer_client"
  )
}

#' @export
print.alchemer_client <- function(x, ...) {
  cli::cli_text("<alchemer_client> {x$base_url}, {x$rpm} req/min")
  invisible(x)
}

# Strip query parameters (which carry credentials) from a URL, keeping only
# scheme + host + path, for anything that might end up in a message or log.
redact_url <- function(url) {
  parts <- httr2::url_parse(url)
  parts$query <- NULL
  httr2::url_build(parts)
}

# Build a throttled, retrying request against one API path. `query` supplies
# path-specific parameters (filters, pagination); credentials are always
# added here, never by the caller.
alchemer_request <- function(client, path, query = list()) {
  req <- httr2::request(paste(client$base_url, path, sep = "/"))
  req <- httr2::req_url_query(req, api_token = client$token, api_token_secret = client$secret)
  if (length(query) > 0) req <- httr2::req_url_query(req, !!!query)
  req <- httr2::req_throttle(req, capacity = client$rpm, fill_time_s = 60, realm = client$base_url)
  httr2::req_retry(
    req,
    max_tries = 5,
    backoff = \(n) 2^n,
    is_transient = is_transient_status,
    retry_on_failure = TRUE
  )
}

# Retried per ADR-011: 429 (rate limit / duplicate-GET cache window) and the
# 5xx family. Every other status is treated as a permanent failure.
is_transient_status <- function(resp) {
  httr2::resp_status(resp) %in% c(429, 500, 502, 503, 504)
}

# Perform a request, catching every httr2 failure (network or HTTP status)
# and re-throwing a condition that carries only method, redacted path, and
# status — never the underlying condition, which may embed the full URL.
alchemer_perform <- function(req) {
  tryCatch(
    httr2::req_perform(req),
    httr2_error = function(e) {
      status <- tryCatch(httr2::resp_status(e$resp), error = function(...) NA_integer_)
      path <- redact_url(req$url)
      cli::cli_abort(
        c(
          "Alchemer API request failed.",
          "i" = "{req$method %||% 'GET'} {path}",
          "i" = "HTTP status: {status %||% 'unknown (connection failure)'}"
        ),
        class = "alchemeR_api_error",
        status = status,
        path = path,
        call = NULL
      )
    }
  )
}

# Alchemer's envelope is inconsistent: most endpoints wrap the payload in a
# `data` key, but several single-object GETs (surveypage-get,
# surveyresponse-get) return fields at the top level instead. This unwraps
# either shape into a plain list, and turns `result_ok: false` into a typed
# error carrying the API's own code/message.
unwrap_envelope <- function(body, req) {
  if (isTRUE(body$result_ok) || is.null(body$result_ok)) {
    body$result_ok <- NULL
    if (!is.null(body$data)) return(body$data)
    return(body)
  }
  cli::cli_abort(
    c(
      "Alchemer API returned an error.",
      "i" = "{req$method %||% 'GET'} {redact_url(req$url)}",
      "i" = "Code: {body$code %||% 'unknown'}",
      "i" = "Message: {body$message %||% 'none'}"
    ),
    class = "alchemeR_api_result_error",
    code = body$code,
    call = NULL
  )
}

#' Fetch a single Alchemer resource
#'
#' Performs one request and returns the parsed payload as a plain (unsimplified)
#' list, regardless of which of Alchemer's two envelope shapes the endpoint uses.
#'
#' @param client An `alchemer_client` from [alchemer_client()].
#' @param path API path relative to `/v5`, e.g. `"survey/123"`.
#' @param query Named list of additional query parameters.
#' @return A list.
#' @keywords internal
alchemer_fetch <- function(client, path, query = list()) {
  req <- alchemer_request(client, path, query)
  resp <- alchemer_perform(req)
  body <- httr2::resp_body_json(resp, simplifyVector = FALSE)
  unwrap_envelope(body, req)
}

# Is this the last page? Prefer total_pages/page when the endpoint returns
# them; otherwise (e.g. contactlist, which sometimes omits them) fall back to
# "this page was empty", which is what the original hand-rolled loop did.
page_is_complete <- function(resp) {
  body <- httr2::resp_body_json(resp, simplifyVector = FALSE)
  data <- body$data
  if (is.null(data) || length(data) == 0) {
    return(TRUE)
  }
  if (!is.null(body$total_pages) && !is.null(body$page)) {
    return(as.numeric(body$page) >= as.numeric(body$total_pages))
  }
  FALSE
}

#' Fetch every page of an Alchemer list endpoint
#'
#' Paginates via [httr2::req_perform_iterative()] and
#' [httr2::iterate_with_offset()] rather than a hand-rolled loop (ADR-011),
#' at `resultsperpage = 500` (the documented maximum).
#'
#' @inheritParams alchemer_fetch
#' @param resultsperpage Page size, max 500.
#' @return A list of the combined `data` elements across every page.
#' @keywords internal
alchemer_fetch_all <- function(client, path, query = list(), resultsperpage = 500) {
  query$resultsperpage <- resultsperpage
  req <- alchemer_request(client, path, query)

  resps <- tryCatch(
    httr2::req_perform_iterative(
      req,
      next_req = httr2::iterate_with_offset("page", resp_complete = page_is_complete),
      max_reqs = Inf,
      progress = FALSE
    ),
    httr2_error = function(e) {
      status <- tryCatch(httr2::resp_status(e$resp), error = function(...) NA_integer_)
      cli::cli_abort(
        c(
          "Alchemer API request failed.",
          "i" = "{req$method %||% 'GET'} {redact_url(req$url)}",
          "i" = "HTTP status: {status %||% 'unknown (connection failure)'}"
        ),
        class = "alchemeR_api_error",
        status = status,
        call = NULL
      )
    }
  )

  items <- purrr::map(resps, function(resp) {
    body <- httr2::resp_body_json(resp, simplifyVector = FALSE)
    unwrap_envelope(body, req)
  })
  purrr::list_flatten(items)
}
