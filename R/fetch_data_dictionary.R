#' Fetch Alchemer Data Dictionary
#'
#' A function that retrieves a data dictionary from an Alchemer survey.
#' Requires user to have set up an API with Alchemer, including
#' a token and secret key. If custom reporting values are used in Alchemer, then
#' the function will download the custom value.
#'
#' @param survey_id
#' Alchemer survey number, retrieved from survey URL

#' @param token
#' Alchemer API token (https://apihelp.alchemer.com/help/authentication).
#' Defaults to the environment variable `ALCHEMER_API_TOKEN`.
#'
#' @param secret_key
#' Alchemer API secret key. Defaults to the environment variable
#' `ALCHEMER_API_SECRET`.

#' @return
#' A tibble with one row per answer option: `question_id` (the column name in
#' the survey data), `qid`, `parent`, `type`, `base_type`, `r_type`, `label`,
#' `option_id`, `option_label`, `option_value`, `page` and `order`.
#' @export
#'
#' @examples
#' \dontrun{
#' fetch_data_dictionary("SURVEY_ID")
#' }

fetch_data_dictionary <- function(
  survey_id,
  token = Sys.getenv("ALCHEMER_API_TOKEN"),
  secret_key = Sys.getenv("ALCHEMER_API_SECRET")
) {
  dictionary_from_definition(get_definition(survey_id, token, secret_key))
}
