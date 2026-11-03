#' Fetch Alchemer Survey Data
#'
#' This function retrieves survey data from an Alchemer account and saves a
#' .csv file in a project folder. Requires user to have set up an API with
#' Alchemer, including a token and secret key
#'
#' @param survey_id
#' Alchemer survey number, retrieved from survey URL
#'
#' @param token
#' Alchemer API token (https://apihelp.alchemer.com/help/authentication).
#' Defaults to the environment variable `ALCHEMER_API_TOKEN`.
#'
#' @param secret_key
#' Alchemer API secret key. Defaults to the environment variable
#' `ALCHEMER_API_SECRET`.
#'
#' @param survey_name
#' Name of survey
#'
#' @return
#' Downloaded file of survey responses
#' @export
#'
#' @examples
#' \dontrun{
#' fetch_survey("SURVEY_ID", survey_name = "SURVEY_NAME")
#' }

fetch_survey <- function(
  survey_id,
  token = Sys.getenv("ALCHEMER_API_TOKEN"),
  secret_key = Sys.getenv("ALCHEMER_API_SECRET"),
  survey_name = "survey_data"
) {
  pages <- api_get_all(
    glue::glue("survey/{survey_id}/surveyresponse"),
    token = token,
    secret_key = secret_key
  )

  vars <- c("id", "status", "is_test_data", "date_submitted", "response_time")

  # Nothing is written until every page has arrived and parsed.
  new_df <- pages |>
    lapply(\(page) {
      data <- page$data
      if (NROW(data) == 0) {
        return(NULL)
      }
      is_sso <- any(stringr::str_detect(names(data), "ssoconfirmation"))
      if (is_sso) {
        dplyr::select(
          data,
          tidyr::all_of(vars),
          tidyr::starts_with("url_variable"),
          tidyr::ends_with("answer")
        )
      } else {
        dplyr::select(data, tidyr::all_of(vars), tidyr::ends_with("answer"))
      }
    }) |>
    dplyr::bind_rows()

  if (nrow(new_df) == 0) {
    stop("Survey ", survey_id, " has no responses.", call. = FALSE)
  }

  names(new_df) <- stringr::str_replace_all(names(new_df), "survey_data.", "Q")
  names(new_df) <- stringr::str_replace_all(names(new_df), ".answer", "")

  utils::write.csv(
    new_df,
    file = glue::glue("{survey_name}.csv"),
    row.names = FALSE
  )
}
