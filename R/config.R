# All configuration comes from the environment, and only from the environment
# (ADR-019). No function in this package takes a credential, a domain, a
# timezone, or a tuning value as an argument: there is exactly one place a
# setting can come from, so "why did this run behave differently?" is always
# answered by the environment it ran in.
#
# A secret still does not have to be written down. A script sets the variable
# from whatever vault it already uses -- keyring::key_get(), say -- before
# calling anything here, which keeps the secret out of project source without
# this package needing to know that vaults exist. See vignette("scheduling").

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

# The shared shape of "this setting has no usable value": name the variable,
# and say what to set it to.
abort_unset <- function(variable, what, hint = NULL) {
  cli::cli_abort(
    c(
      "No {what} configured.",
      "i" = "Set the {.envvar {variable}} environment variable.",
      hint
    ),
    class = "alchemeR_config_error", call = NULL
  )
}

#' Alchemer API credentials
#'
#' Reads `ALCHEMER_API_TOKEN` and `ALCHEMER_API_SECRET` from the environment.
#' To keep a secret out of project source, set the variable from a vault in
#' your script: `Sys.setenv(ALCHEMER_API_SECRET = keyring::key_get(...))`.
#'
#' @return A list with `token` and `secret`.
#' @keywords internal
alchemer_creds <- function() {
  token <- env_or("ALCHEMER_API_TOKEN", "")
  secret <- env_or("ALCHEMER_API_SECRET", "")

  if (!nzchar(token)) {
    abort_unset("ALCHEMER_API_TOKEN", "Alchemer API token")
  }
  if (!nzchar(secret)) {
    abort_unset(
      "ALCHEMER_API_SECRET", "Alchemer API secret",
      hint = c("i" = "From a vault: {.code Sys.setenv(ALCHEMER_API_SECRET =
                keyring::key_get(\"alchemer\", \"api_secret\"))}.")
    )
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
#' @return A single string, e.g. `"api.alchemer.com"`.
#' @keywords internal
alchemer_domain <- function() {
  env_or("ALCHEMER_DOMAIN", "api.alchemer.com")
}

#' Alchemer application database directory
#'
#' Reads `ALCHEMER_DB`. Every pipeline function defaults its `db` argument to
#' this, and it is exported so that opening your own connection (see
#' `vignette("getting-started")`) gives the same actionable error when
#' `ALCHEMER_DB` isn't set, rather than a raw "object not found".
#'
#' @return A single string path.
#' @export
alchemer_db_path <- function() {
  db <- env_or("ALCHEMER_DB", "")
  if (!nzchar(db)) {
    abort_unset(
      "ALCHEMER_DB", "application database directory",
      hint = c("i" = "Use an absolute path for a scheduled job: its working directory
                is not guaranteed.")
    )
  }
  db
}

#' Alchemer API request throttle ceiling
#'
#' Requests per minute. Defaults to `ALCHEMER_RPM`, or 100 — below the
#' documented account-wide limit of 240/min, leaving headroom for other,
#' interactive use of the same account (ADR-011).
#'
#' @return A single number.
#' @keywords internal
alchemer_rpm <- function() {
  as.numeric(env_or("ALCHEMER_RPM", "100"))
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
#' @return A single IANA timezone name, e.g. `"America/Toronto"`.
#' @keywords internal
alchemer_tz <- function() {
  # Sys.timezone() returns NA when the OS zone can't be determined, so it
  # needs or_default() rather than %||% behind it.
  tz <- env_or("ALCHEMER_TZ", or_default(Sys.timezone(), "UTC"))
  # Validated against the IANA database rather than quoted in context: this
  # reaches SQL as a bare `AT TIME ZONE '<tz>'` literal, and an unrecognised
  # name is a configuration mistake worth naming up front rather than a
  # confusing DuckDB error thrown once per generated statement.
  if (!isTRUE(tz %in% OlsonNames())) {
    cli::cli_abort(
      c(
        "{.val {tz}} is not a known timezone name.",
        "i" = "Set {.envvar ALCHEMER_TZ} to an IANA name such as {.val America/Toronto}.",
        "i" = "See {.run OlsonNames()} for the full list."
      ),
      class = "alchemeR_config_error", call = NULL
    )
  }
  tz
}

#' Title language for the publication layer
#'
#' Which language multilingual titles are resolved to in `pub`. Defaults to
#' `ALCHEMER_LANGUAGE`, then `"English"`. Changing it makes the next
#' [pub_layer()] rebuild every survey, since it changes the values written
#' from identical input (ADR-017).
#'
#' @return A single string, e.g. `"English"`.
#' @keywords internal
alchemer_language <- function() {
  language <- env_or("ALCHEMER_LANGUAGE", "English")
  # Validated up front for the same reason as the timezone above: title_sql()
  # interpolates this straight into a generated SQL/JSONPath expression with
  # no further escaping. Alchemer language names are always simple words.
  if (!grepl("^[A-Za-z0-9 _-]+$", language)) {
    cli::cli_abort(
      c(
        "{.envvar ALCHEMER_LANGUAGE} must contain only letters, digits, spaces,
         '-', or '_'; got {.val {language}}.",
        "i" = "It is the language name Alchemer uses for a title, e.g. {.val English}."
      ),
      class = "alchemeR_config_error", call = NULL
    )
  }
  language
}

#' Maximum staleness backstop for survey refresh
#'
#' A correctness backstop, not a freshness setting (ADR-004): change
#' detection already catches new responses, edits, and deletions, so this
#' only bounds how long an undetected change could persist.
#'
#' @return A single number.
#' @keywords internal
alchemer_full_sweep_days <- function() {
  as.numeric(env_or("ALCHEMER_FULL_SWEEP_DAYS", "90"))
}
