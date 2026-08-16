# Deprecated shims for the three functions this package previously exposed.
# Each delegates to its refactored replacement in api_surveys.R and warns
# once per session (lifecycle::deprecate_warn()). They will be removed in
# 2.0.0 (NEWS.md).

# These are the only functions in the package that still accept credentials as
# arguments. That is deliberate and does not reopen ADR-019: a deprecated
# function's entire job is to let calls written against the old API keep
# working, and the old API took a token and a secret_key. Everything current
# reads the environment and nothing else. Unsupplied, these fall back to the
# environment too, with the same error as every other entry point.
deprecated_client <- function(token, secret_key) {
  token <- token %||% env_or("ALCHEMER_API_TOKEN", "")
  secret <- secret_key %||% env_or("ALCHEMER_API_SECRET", "")
  if (!nzchar(token)) {
    abort_unset("ALCHEMER_API_TOKEN", "Alchemer API token")
  }
  if (!nzchar(secret)) {
    abort_unset("ALCHEMER_API_SECRET", "Alchemer API secret")
  }
  new_alchemer_client(token, secret)
}

#' Retrieve all surveys in Alchemer
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' Superseded by [alchemer_surveys()], which reads credentials from the
#' environment by default and returns the same tibble shape as every other
#' direct-API function in this package. **This is not a drop-in replacement
#' for the output shape**: nested fields such as `statistics` are no longer
#' flattened to dotted column names (`statistics.Complete`) the way the old
#' `jsonlite::fromJSON(flatten = TRUE)`-based implementation did -- they come
#' back as list-columns instead.
#'
#' @param token Alchemer API token. Defaults to `ALCHEMER_API_TOKEN`.
#' @param secret_key Alchemer API secret key. Defaults to `ALCHEMER_API_SECRET`.
#' @return A tibble with all Alchemer surveys.
#' @export
all_surveys <- function(token = NULL, secret_key = NULL) {
  lifecycle::deprecate_warn("1.0.0", "all_surveys()", "alchemer_surveys()")
  alchemer_surveys(deprecated_client(token, secret_key))
}

#' Fetch Alchemer survey response data
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' Superseded by [alchemer_responses()]. Unlike the original, this no
#' longer writes a CSV file unless `file` is supplied explicitly, and when it
#' does, **the file's columns are different**: one `survey_data` column
#' holding each response's answers as a JSON string, not one `Q<id>` column
#' per question. A script that reads the old wide-format CSV will need
#' updating, not just adding `file =`.
#'
#' @param survey_id Alchemer survey number, retrieved from the survey URL.
#' @param token Alchemer API token. Defaults to `ALCHEMER_API_TOKEN`.
#' @param secret_key Alchemer API secret key. Defaults to `ALCHEMER_API_SECRET`.
#' @param survey_name Unused; retained for signature compatibility.
#' @param file If supplied, a path to write the responses to as a CSV.
#' @return A tibble of survey responses, invisibly if `file` is supplied.
#' @export
fetch_survey <- function(survey_id, token = NULL, secret_key = NULL,
                         survey_name = "survey_data", file = NULL) {
  lifecycle::deprecate_warn("1.0.0", "fetch_survey()", "alchemer_responses()")
  df <- alchemer_responses(survey_id, deprecated_client(token, secret_key))
  if (!is.null(file)) {
    # alchemer_responses() keeps nested fields (survey_data, url_variables,
    # data_quality) as list-columns (ADR-003 applies to raw fidelity
    # generally); a CSV can't hold those, so they're serialised to JSON
    # strings for the file only. The returned tibble keeps the list-columns.
    flat <- dplyr::mutate(df, dplyr::across(
      dplyr::where(is.list),
      \(col) vapply(col, jsonlite::toJSON, character(1), auto_unbox = TRUE)
    ))
    utils::write.csv(flat, file = file, row.names = FALSE)
    return(invisible(df))
  }
  df
}

#' Fetch an Alchemer survey's data dictionary
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' Superseded by [alchemer_questions()]. **This is not a drop-in replacement
#' for the output shape**: the old return columns
#' (`question_id`/`label`/`options_id`/`options_value`) are replaced by
#' [alchemer_questions()]'s columns (`id`, `title`, `shortname`, `varname`,
#' ...). Code that joined the old output to `fetch_survey()`'s
#' `Q<id>`-prefixed columns by `question_id` will need to be rewritten, not
#' just have its deprecation warning silenced.
#'
#' @param survey_id Alchemer survey number, retrieved from the survey URL.
#' @param token Alchemer API token. Defaults to `ALCHEMER_API_TOKEN`.
#' @param secret_key Alchemer API secret key. Defaults to `ALCHEMER_API_SECRET`.
#' @return A tibble of question metadata.
#' @export
fetch_data_dictionary <- function(survey_id, token = NULL, secret_key = NULL) {
  lifecycle::deprecate_warn("1.0.0", "fetch_data_dictionary()", "alchemer_questions()")
  alchemer_questions(survey_id, deprecated_client(token, secret_key))
}
