# Read an environment variable, falling back to `default` when it is either
# unset *or* set to the empty string. The empty case matters: Renviron.example
# ships every optional setting as a `NAME=value` line, so a user who blanks
# one out (rather than deleting the line) would otherwise get "" rather than
# the documented default -- which produced a "https:///v5" base URL and a
# `capacity must be a whole number, not NA` throttle error rather than
# anything that names the real problem.
env_or <- function(name, default) {
  value <- Sys.getenv(name, "")
  if (nzchar(value)) value else default
}

#' Alchemer API credentials
#'
#' Reads `ALCHEMER_API_TOKEN` and `ALCHEMER_API_SECRET` from the environment.
#' Either may be supplied directly as an argument instead (so, e.g., `keyring`
#' users can source the secret without ever writing it to project source).
#'
#' @param token API token. Defaults to `Sys.getenv("ALCHEMER_API_TOKEN")`.
#' @param secret API secret. Defaults to `Sys.getenv("ALCHEMER_API_SECRET")`.
#' @return A list with `token` and `secret`.
#' @keywords internal
alchemer_creds <- function(token = NULL, secret = NULL) {
  token <- token %||% env_or("ALCHEMER_API_TOKEN", "")
  secret <- secret %||% env_or("ALCHEMER_API_SECRET", "")

  if (!nzchar(token)) {
    cli::cli_abort(c(
      "Alchemer API token not found.",
      "i" = "Set the {.envvar ALCHEMER_API_TOKEN} environment variable, or pass {.arg token} directly."
    ), class = "alchemeR_config_error")
  }
  if (!nzchar(secret)) {
    cli::cli_abort(c(
      "Alchemer API secret not found.",
      "i" = "Set the {.envvar ALCHEMER_API_SECRET} environment variable, or pass {.arg secret} directly."
    ), class = "alchemeR_config_error")
  }

  list(token = token, secret = secret)
}

#' Alchemer regional API base domain
#'
#' `ALCHEMER_DOMAIN` defaults to `api.alchemer.com`. The current published
#' package hardcodes the Canadian domain (`api.alchemer-ca.com`), which fails
#' for every account outside that region; the API docs call a wrong-domain
#' call "one of the most common causes of failed API calls".
#'
#' @param domain Domain override. Defaults to `Sys.getenv("ALCHEMER_DOMAIN")`.
#' @return A single string, e.g. `"api.alchemer.com"`.
#' @keywords internal
alchemer_domain <- function(domain = NULL) {
  domain %||% env_or("ALCHEMER_DOMAIN", "api.alchemer.com")
}

#' Alchemer application database directory
#'
#' Every pipeline function defaults its `db` argument to this. Also handy for
#' opening your own connection (see `vignette("getting-started")`), since it
#' gives the same actionable error as the pipeline functions when
#' `ALCHEMER_DB` isn't set, rather than a raw "object not found".
#'
#' @param db Directory override. Defaults to `Sys.getenv("ALCHEMER_DB")`.
#' @return A single string path.
#' @export
alchemer_db_path <- function(db = NULL) {
  db <- db %||% env_or("ALCHEMER_DB", "")
  if (!nzchar(db)) {
    cli::cli_abort(c(
      "No application database directory configured.",
      "i" = "Set the {.envvar ALCHEMER_DB} environment variable, or pass {.arg db} directly."
    ), class = "alchemeR_config_error")
  }
  db
}

#' Alchemer API request throttle ceiling
#'
#' Requests per minute. Defaults to `ALCHEMER_RPM`, or 100 — below the
#' documented account-wide limit of 240/min, leaving headroom for other,
#' interactive use of the same account (ADR-011).
#'
#' @param rpm Override. Defaults to `Sys.getenv("ALCHEMER_RPM")`.
#' @return A single number.
#' @keywords internal
alchemer_rpm <- function(rpm = NULL) {
  as.numeric(rpm %||% env_or("ALCHEMER_RPM", "100"))
}

#' Timezone for the publication layer
#'
#' The timezone `pub`'s timestamps are presented in, and the timezone
#' Alchemer's own unsuffixed timestamps are read as. `raw` is never
#' converted (ADR-003) -- it keeps whatever string the API sent.
#'
#' Defaults to `ALCHEMER_TZ`, then to the machine's own timezone
#' (`Sys.timezone()`). **Set it explicitly for a scheduled job**: leaving it
#' unset makes `pub` depend on the timezone of whichever machine last ran
#' [pub_layer()], so a laptop and a UTC server would write different values
#' into the same shared database.
#'
#' @param tz Override. Defaults to `Sys.getenv("ALCHEMER_TZ")`.
#' @return A single IANA timezone name, e.g. `"America/Toronto"`.
#' @keywords internal
alchemer_tz <- function(tz = NULL) {
  # Sys.timezone() returns NA when the OS zone can't be determined, so it
  # needs or_default() rather than %||% behind it.
  tz <- tz %||% env_or("ALCHEMER_TZ", or_default(Sys.timezone(), "UTC"))
  # Validated against the IANA database rather than quoted in context: this
  # reaches SQL as a bare `AT TIME ZONE '<tz>'` literal, and an unrecognised
  # name is a configuration mistake worth naming up front rather than a
  # confusing DuckDB error thrown once per generated statement.
  if (!isTRUE(tz %in% OlsonNames())) {
    cli::cli_abort(c(
      "{.val {tz}} is not a known timezone name.",
      "i" = "Set {.envvar ALCHEMER_TZ} to an IANA name such as {.val America/Toronto}.",
      "i" = "See {.run OlsonNames()} for the full list."
    ), class = "alchemeR_config_error")
  }
  tz
}

#' Maximum staleness backstop for survey refresh
#'
#' A correctness backstop, not a freshness setting (ADR-004): change
#' detection already catches new responses, edits, and deletions, so this
#' only bounds how long an undetected change could persist.
#'
#' @param days Override. Defaults to `Sys.getenv("ALCHEMER_FULL_SWEEP_DAYS")`.
#' @return A single number.
#' @keywords internal
alchemer_full_sweep_days <- function(days = NULL) {
  as.numeric(days %||% env_or("ALCHEMER_FULL_SWEEP_DAYS", "90"))
}
