#' Retrieve all surveys in Alchemer
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
#' Data frame with all Alchemer surveys (up to 500; an error is raised if the
#' account has more).
#' @export
#'
#' @examples
#' \dontrun{
#' all_surveys()
#' }
all_surveys <- function(
  token = Sys.getenv("ALCHEMER_API_TOKEN"),
  secret_key = Sys.getenv("ALCHEMER_API_SECRET")
) {
  surveys <- api_get(
    "survey",
    list(resultsperpage = 500, page = 1),
    token,
    secret_key
  )

  if (surveys$total_pages > 1) {
    stop(
      "The account has more than 500 surveys; all_surveys() reads one page.",
      call. = FALSE
    )
  }

  surveys$data
}
