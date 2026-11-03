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
#' A data frame of question numbers and question labels
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
  pages <- api_get_all(
    glue::glue("survey/{survey_id}/surveyquestion"),
    token = token,
    secret_key = secret_key
  )
  temp_df <- dplyr::bind_rows(lapply(pages, \(x) x$data))

  # Coerce all columns in the nested data frames to character vectors
  temp_df$options <- lapply(temp_df$options, function(x) {
    if (is.null(x)) {
      return(NULL)
    }
    as.data.frame(lapply(x, as.character), stringsAsFactors = FALSE)
  })

  temp_df |>
    dplyr::select(id, "label" = title.English, options) |>
    tidyr::unnest(options, names_sep = "_", keep_empty = TRUE) |>
    dplyr::select(
      "question_id" = id,
      label,
      options_id,
      options_value
    ) |>
    dplyr::mutate(question_id = glue::glue("Q{question_id}"))
}
