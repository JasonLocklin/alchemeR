#' Fetch Alchemer Survey Data
#'
#' This function retrieves survey data from an Alchemer account and saves a .csv file in a project folder.
#' Requires user to have set up an API with Alchemer, including a token and secret key
#'
#' @param survey_id
#' Alchemer survey number, retrieved from survey URL
#'
#' @param token
#' Alchemer API token (https://apihelp.alchemer.com/help/authentication)
#'
#' @param secret_key
#' Alchemer API secret key
#'
#' @param survey_name
#' Name of survey
#'
#' @return
#' Downloaded file of survey responses
#' @export
#'
#' @examples
#' # alchemer_survey("SURVEY_ID", <YOUR-ALCHEMER_TOKEN>, <YOUR-ALCHEMER_SECRET_KEY>, "SURVEY_NAME")

fetch_survey <- function(survey_id, token = "token", secret_key = "secret_key", survey_name = "survey_data"){

  survey_url <- glue::glue("https://api.alchemer-ca.com/v5/survey/{survey_id}/surveyresponse")
  survey_req <- httr2::request(survey_url)

  vars <- c( "id", "status", "is_test_data", "date_submitted", "response_time")

  for (i in c(1:300)) {

    survey <- survey_req |>
      httr2::req_url_query(
        `api_token` = token,
        `api_token_secret` = secret_key,
        `resultsperpage` = 250,
        page = i
      )

    temp_survey_df <- jsonlite::fromJSON(survey$url, flatten = TRUE)

    is_sso <- any(grepl("ssoconfirmation", names(temp_survey_df$data), fixed = TRUE))

    if (length(temp_survey_df$data) == 0) {
      break
    } else {
      if(is_sso == TRUE){
        temp_df <- temp_survey_df$data |>
          dplyr::select(tidyr::all_of(vars),
                        tidyr::starts_with("url_variable"),
                        tidyr::ends_with("answer"))
      } else{
        temp_df <- temp_survey_df$data |>
          dplyr::select(tidyr::all_of(vars), tidyr::ends_with("answer"))
      }
    }

    if(!exists("new_df")){
      new_df <- temp_df
    } else {
      new_df <- new_df |>
        dplyr::bind_rows(temp_df)
    }
  }

  names(new_df) <- gsub("survey_data.", "Q", names(new_df), fixed = TRUE)
  names(new_df) <- gsub(".answer", "", names(new_df), fixed = TRUE)

  utils::write.csv(
    new_df,
    file = glue::glue("{survey_name}.csv"),
    row.names = FALSE
  )
}


