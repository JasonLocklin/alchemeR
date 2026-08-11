#' Fetch Alchemer Data Dictionary
#'
#' A function that retrieves a data dictionary from an Alchemer survey.
#' Requires user to have set up an API with Alchemer, including
#' a token and secret key. If custom reporting values are used in Alchemer, then
#' the function will download the custome value.
#'
#' @param survey_id
#' Alchemer survey number, retrieved from survey URL

#' @param token
#' Alchemer API token (https://apihelp.alchemer.com/help/authentication)
#'
#' @param secret_key
#' Alchemer API secret key

#' @return
#' A data frame of question numbers and question labels
#' @export
#'
#' @examples
#' # fetch_data_dictionary("SURVEY_ID","YOUR-ALCHEMER_TOKEN","YOUR-ALCHEMER_SECRET_KEY")

fetch_data_dictionary <- function(survey_id, token = "token", secret_key = "secret_key"){

  survey_url <- glue::glue("https://api.alchemer-ca.com/v5/survey/{survey_id}/surveyquestion")
  survey_req <- httr2::request(survey_url)

  survey <- survey_req |>
    httr2::req_url_query(
      `api_token` = token,
      `api_token_secret` = secret_key,
      `resultsperpage` = 25,
      page = 1
    )

  temp_df <- jsonlite::fromJSON(survey$url, flatten = TRUE)$data
  
  # Coerce all columns in the nested data frames to character vectors
  temp_df$options <- lapply(temp_df$options, function(x) {
    if (is.null(x)) return(NULL)
    as.data.frame(lapply(x, as.character), stringsAsFactors = FALSE)
  })

  temp_df |>
    dplyr::select(.data$id,
                  "label" = .data$title.English,
                  .data$options) |>
    tidyr::unnest(options,
                  names_sep = "_",
                  keep_empty = TRUE) |>
    dplyr::select(
      "question_id" = .data$id,
      .data$label,
      .data$options_id,
      .data$options_value) |>
    dplyr::mutate(question_id = glue::glue("Q{.data$question_id}"))

}



