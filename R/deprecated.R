# Deprecated shims for the three functions this package previously exposed.
# Each delegates to its refactored replacement in api_surveys.R and warns
# once per session (lifecycle::deprecate_warn()). They will be removed in
# 2.0.0 (NEWS.md).

#' Retrieve all surveys in Alchemer
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' Superseded by [alchemer_surveys()], which reads credentials from the
#' environment by default and returns the same tibble shape as every other
#' direct-API function in this package.
#'
#' @param token Alchemer API token.
#' @param secret_key Alchemer API secret key.
#' @return A tibble with all Alchemer surveys.
#' @export
all_surveys <- function(token, secret_key) {
  lifecycle::deprecate_warn("1.0.0", "all_surveys()", "alchemer_surveys()")
  alchemer_surveys(alchemer_client(token = token, secret = secret_key))
}

#' Fetch Alchemer survey response data
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' Superseded by [alchemer_responses()]. Unlike the original, this no
#' longer writes a CSV file unless `file` is supplied explicitly.
#'
#' @param survey_id Alchemer survey number, retrieved from the survey URL.
#' @param token Alchemer API token.
#' @param secret_key Alchemer API secret key.
#' @param survey_name Unused; retained for signature compatibility.
#' @param file If supplied, a path to write the responses to as a CSV.
#' @return A tibble of survey responses, invisibly if `file` is supplied.
#' @export
fetch_survey <- function(survey_id, token = "token", secret_key = "secret_key",
                         survey_name = "survey_data", file = NULL) {
  lifecycle::deprecate_warn("1.0.0", "fetch_survey()", "alchemer_responses()")
  df <- alchemer_responses(survey_id, alchemer_client(token = token, secret = secret_key))
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
#' Superseded by [alchemer_questions()].
#'
#' @param survey_id Alchemer survey number, retrieved from the survey URL.
#' @param token Alchemer API token.
#' @param secret_key Alchemer API secret key.
#' @return A tibble of question metadata.
#' @export
fetch_data_dictionary <- function(survey_id, token = "token", secret_key = "secret_key") {
  lifecycle::deprecate_warn("1.0.0", "fetch_data_dictionary()", "alchemer_questions()")
  alchemer_questions(survey_id, alchemer_client(token = token, secret = secret_key))
}
