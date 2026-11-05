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
#' A tibble with one row per survey (up to 500; an error is raised if the
#' account has more): `id`, `title`, `status`, `type`, `team` (ids, comma
#' separated), `created_on` and `modified_on` (Toronto time).
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

  rows <- surveys$data
  field <- \(name) vapply(rows, \(x) chr(x[[name]]), "")
  dplyr::tibble(
    id = as.integer(field("id")),
    title = field("title"),
    status = field("status"),
    type = field("type"),
    team = vapply(rows, \(x) paste(unlist(x$team), collapse = ","), ""),
    created_on = parse_time(field("created_on")),
    modified_on = parse_time(field("modified_on"))
  )
}
