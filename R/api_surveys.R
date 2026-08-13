# Refactored direct-API access. Every function here returns a tibble and has
# no file side effects (contrast the deprecated shims in deprecated.R, which
# preserve the old file-writing behaviour only when explicitly asked for).
# All paginate fully via alchemer_fetch_all() / httr2 (ADR-011).

#' List every survey in the account
#'
#' @param client An `alchemer_client`, from [alchemer_client()]. Defaults to
#'   one built from the environment (`ALCHEMER_API_TOKEN`, `ALCHEMER_API_SECRET`).
#' @return A tibble, one row per survey.
#' @export
alchemer_surveys <- function(client = alchemer_client()) {
  items_to_tibble(alchemer_fetch_all(client, "survey"))
}

#' Fetch one survey's full definition tree
#'
#' A single request returns the survey's pages, questions, and options
#' nested inline (`GET /v5/survey/{id}`) — this is `alchemeR`'s preferred way
#' to read a survey's structure, rather than crawling the SurveyPage/
#' SurveyQuestion/SurveyOption list endpoints separately.
#'
#' @param survey_id Alchemer survey id.
#' @inheritParams alchemer_surveys
#' @return A one-row tibble. Nested `pages`/`questions`/`options` are kept as
#'   a list-column (`pages`) holding the tree exactly as the API returned it.
#' @export
alchemer_survey <- function(survey_id, client = alchemer_client()) {
  items_to_tibble(list(alchemer_fetch(client, glue::glue("survey/{survey_id}"))))
}

#' List a survey's questions
#'
#' @inheritParams alchemer_survey
#' @return A tibble, one row per question.
#' @export
alchemer_questions <- function(survey_id, client = alchemer_client()) {
  items_to_tibble(alchemer_fetch_all(client, glue::glue("survey/{survey_id}/surveyquestion")))
}

#' List a survey's responses
#'
#' Downloads **all** responses (paginated at 500/page), not a filtered
#' subset — matching `alchemeR`'s full-refresh design (ADR-004). Pass
#' Alchemer's documented filter/order_by query parameters via `...` to
#' narrow the request, e.g. `order_by = "-date_updated"`.
#'
#' @inheritParams alchemer_survey
#' @param ... Additional query parameters (e.g. `order_by`, `resultsperpage`,
#'   or `` `filter[field][0]` `` style filters) passed through to the API.
#' @return A tibble, one row per response.
#' @export
alchemer_responses <- function(survey_id, client = alchemer_client(), ...) {
  items_to_tibble(alchemer_fetch_all(client, glue::glue("survey/{survey_id}/surveyresponse"), query = list(...)))
}

#' List a survey's campaigns (distribution links)
#'
#' @inheritParams alchemer_survey
#' @return A tibble, one row per campaign.
#' @export
alchemer_campaigns <- function(survey_id, client = alchemer_client()) {
  items_to_tibble(alchemer_fetch_all(client, glue::glue("survey/{survey_id}/surveycampaign")))
}

#' List a survey's question-level statistics
#'
#' @inheritParams alchemer_survey
#' @return A tibble, one row per question. Non-answerable question types
#'   (INSTRUCTIONS, ESSAY, GROUP, SCRIPT, HIDDEN) carry only `id`/`type`.
#' @export
alchemer_statistics <- function(survey_id, client = alchemer_client()) {
  items_to_tibble(alchemer_fetch_all(client, glue::glue("survey/{survey_id}/surveystatistic")))
}
