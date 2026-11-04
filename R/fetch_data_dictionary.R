#' Fetch Alchemer Data Dictionary
#'
#' Reads the survey definition (one request) and returns the data dictionary:
#' one row per answer option of every question that holds an answer, in
#' questionnaire order. It is the same dictionary that [fetch_survey()] stores
#' in `attr(x, "alchemer")$dictionary`. Requires an Alchemer API token and
#' secret key.
#'
#' @param survey_id
#' Alchemer survey number, retrieved from the survey URL.
#'
#' @param token
#' Alchemer API token (https://apihelp.alchemer.com/help/authentication).
#' Defaults to the environment variable `ALCHEMER_API_TOKEN`.
#'
#' @param secret_key
#' Alchemer API secret key. Defaults to the environment variable
#' `ALCHEMER_API_SECRET`.
#'
#' @return
#' A tibble with one row per answer option: `question_id` (the column name in
#' the survey data), `qid`, `parent` (the table question of a table row),
#' `type`, `base_type`, `r_type` (`"double"` or `"character"`), `label` (the
#' column label), `option_id`, `option_label` (the option text),
#' `option_value` (the reporting value, which is what a choice answer holds in
#' the survey data), `page` and `order`.
#' @export
#'
#' @examples
#' \dontrun{
#' dict <- fetch_data_dictionary(123)
#' }
fetch_data_dictionary <- function(
  survey_id,
  token = Sys.getenv("ALCHEMER_API_TOKEN"),
  secret_key = Sys.getenv("ALCHEMER_API_SECRET")
) {
  survey_id <- check_survey_id(survey_id)
  dictionary_from_definition(get_definition(survey_id, token, secret_key))
}
