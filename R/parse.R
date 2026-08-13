# Pure functions: raw API list -> tibble matching a raw.* table's columns.
# No I/O, no network, no DB access -- everything here is testable against
# fixtures alone. Every output column is character or JSON text (ADR-003);
# a field this package doesn't know about yet is never dropped (it survives
# in `payload`) and never causes an error, only an NA in a named column.

# Redacted at ingest, everywhere they appear -- including inside `payload`
# (ADR-009). A single auditable list, not scattered inline conditionals.
pii_keys <- c(
  "ip_address", "latitude", "longitude",
  "country", "city", "region", "postal", "dma",
  "user_agent"
)

# NULL, empty list, and "field absent" all collapse to NA for a scalar
# column; anything else is coerced to a length-1 character.
chr1 <- function(x) {
  if (is.null(x) || (is.list(x) && length(x) == 0)) {
    return(NA_character_)
  }
  as.character(x)
}

# A nested field (object/array) becomes JSON text. An absent field (`NULL`)
# becomes SQL NULL rather than the string "null"; an empty array or object
# that Alchemer actually sent is kept as the `[]`/`{}` it sent, since "the
# API returned an empty list" and "the API didn't return this field" are
# different facts and `raw` records what arrived (ADR-003).
json1 <- function(x, redact = character(0)) {
  if (is.null(x)) {
    return(NA_character_)
  }
  if (length(redact) > 0 && is.list(x)) {
    x[redact] <- NULL
  }
  as.character(jsonlite::toJSON(x, auto_unbox = TRUE, null = "null"))
}

#' Parse a `GET /v5/survey` list into `raw.surveys` rows
#' @param items Result of `alchemer_fetch_all(client, "survey")`.
#' @return A tibble matching `raw.surveys`' columns.
#' @keywords internal
parse_surveys <- function(items) {
  # No early-return-tibble() shortcut for an empty `items`: purrr::map_chr()
  # over list() already yields character(0), so this produces a correctly
  # full-column, zero-row tibble on its own. A bare tibble::tibble() (no
  # columns at all) previously broke upsert_surveys(), which combines this
  # result with previously-stored rows and subsets by column name.
  tibble::tibble(
    survey_id = purrr::map_chr(items, ~ chr1(.x$id)),
    title = purrr::map_chr(items, ~ chr1(.x$title)),
    type = purrr::map_chr(items, ~ chr1(.x$type)),
    status = purrr::map_chr(items, ~ chr1(.x$status)),
    created_on = purrr::map_chr(items, ~ chr1(.x$created_on)),
    modified_on = purrr::map_chr(items, ~ chr1(.x$modified_on)),
    team = purrr::map_chr(items, ~ chr1(.x$team)),
    statistics = purrr::map_chr(items, ~ json1(.x$statistics)),
    links = purrr::map_chr(items, ~ json1(.x$links)),
    payload = purrr::map_chr(items, ~ json1(.x))
  )
}

#' Parse one survey's full definition tree into pages/questions/options
#'
#' `def` is the raw result of `alchemer_fetch(client, "survey/{id}")` --
#' pages, questions, and options nested inline. `varname` is absent from
#' this tree (it is only returned by the separate `surveyquestion` list, per
#' ADR discussion in plan.md), so it is merged in from `varnames`, a named
#' character vector `question_id -> varname` built by the caller from that
#' endpoint.
#'
#' @param survey_id Survey id (kept alongside `def$id` in case of mismatch).
#' @param def The parsed definition tree.
#' @param varnames Named character vector, `question_id -> varname`.
#' @return A list with `definition`, `pages`, `questions`, `options` tibbles.
#' @keywords internal
parse_survey_definition <- function(survey_id, def, varnames = character(0)) {
  pages <- list()
  questions <- list()
  options <- list()

  page_list <- def$pages %||% list()
  for (p in seq_along(page_list)) {
    page <- page_list[[p]]
    pages[[length(pages) + 1]] <- tibble::tibble(
      survey_id = survey_id,
      page_id = chr1(page$id),
      title = json1(page$title),
      description = chr1(page$description),
      properties = json1(page$properties),
      page_order = p,
      payload = json1(page[setdiff(names(page), "questions")])
    )

    question_list <- page$questions %||% list()
    for (q in seq_along(question_list)) {
      question <- question_list[[q]]
      question_id <- chr1(question$id)
      questions[[length(questions) + 1]] <- tibble::tibble(
        survey_id = survey_id,
        question_id = question_id,
        page_id = chr1(page$id),
        base_type = chr1(question$base_type),
        type = chr1(question$type),
        title = json1(question$title),
        shortname = chr1(question$shortname),
        varname = unname(varnames[question_id]) %||% NA_character_,
        description = chr1(question$description),
        properties = json1(question$properties),
        show_rules_ids = json1(question$show_rules_ids),
        question_order = q,
        payload = json1(question[setdiff(names(question), "options")])
      )

      option_list <- question$options %||% list()
      for (o in seq_along(option_list)) {
        option <- option_list[[o]]
        options[[length(options) + 1]] <- tibble::tibble(
          survey_id = survey_id,
          question_id = question_id,
          option_id = chr1(option$id),
          title = json1(option$title),
          value = chr1(option$value),
          properties = json1(option$properties),
          option_order = o,
          payload = json1(option)
        )
      }
    }
  }

  list(
    definition = tibble::tibble(survey_id = survey_id, payload = json1(def)),
    pages = dplyr::bind_rows(pages),
    questions = dplyr::bind_rows(questions),
    options = dplyr::bind_rows(options)
  )
}

#' Build a `question_id -> varname` lookup from a `surveyquestion` list
#' @param items Result of `alchemer_fetch_all(client, "survey/{id}/surveyquestion")`.
#' @return A named character vector.
#' @keywords internal
parse_varnames <- function(items) {
  if (length(items) == 0) {
    return(character(0))
  }
  ids <- purrr::map_chr(items, ~ chr1(.x$id))
  varnames <- purrr::map_chr(items, ~ chr1(.x$varname))
  stats::setNames(varnames, ids)
}

#' Parse a `surveyresponse` list into `raw.responses` rows
#'
#' PII fields (ADR-009) are dropped entirely -- they are never stored as
#' columns, and are stripped from `payload` before it is serialised, so no
#' copy of them survives ingestion.
#'
#' @param survey_id Survey id.
#' @param items Result of `alchemer_fetch_all(client, "survey/{id}/surveyresponse")`.
#' @return A tibble matching `raw.responses`' columns.
#' @keywords internal
parse_responses <- function(survey_id, items) {
  # See parse_surveys()'s comment: no early-return-tibble() shortcut for an
  # empty `items`. A survey whose responses have all disappeared is a real
  # state, and merge_survey_rows() has to be able to tell "nothing fetched"
  # (flag every stored row as deleted) from a frame it cannot read at all.
  tibble::tibble(
    survey_id = survey_id,
    response_id = purrr::map_chr(items, ~ chr1(.x$id)),
    status = purrr::map_chr(items, ~ chr1(.x$status)),
    is_test_data = purrr::map_chr(items, ~ chr1(.x$is_test_data)),
    date_submitted = purrr::map_chr(items, ~ chr1(.x$date_submitted)),
    date_started = purrr::map_chr(items, ~ chr1(.x$date_started)),
    date_updated = purrr::map_chr(items, ~ chr1(.x$date_updated)),
    session_id = purrr::map_chr(items, ~ chr1(.x$session_id)),
    language = purrr::map_chr(items, ~ chr1(.x$language)),
    link_id = purrr::map_chr(items, ~ chr1(.x$link_id)),
    contact_id = purrr::map_chr(items, ~ chr1(.x$contact_id)),
    response_time = purrr::map_chr(items, ~ chr1(.x$response_time)),
    url_variables = purrr::map_chr(items, ~ json1(.x$url_variables)),
    data_quality = purrr::map_chr(items, ~ json1(.x$data_quality)),
    survey_data = purrr::map_chr(items, ~ json1(.x$survey_data)),
    payload = purrr::map_chr(items, ~ json1(.x, redact = pii_keys))
  )
}

#' Parse a `surveystatistic` list into `raw.survey_statistics` rows
#'
#' Survives the API's `"total responses"` (space, not underscore) key by
#' keeping every non-id/type field as-is inside `stats` rather than naming
#' them individually.
#'
#' @param survey_id Survey id.
#' @param items Result of `alchemer_fetch_all(client, "survey/{id}/surveystatistic")`.
#' @return A tibble matching `raw.survey_statistics`' columns.
#' @keywords internal
parse_statistics <- function(survey_id, items) {
  # See parse_surveys()'s comment: no early-return-tibble() shortcut for an
  # empty `items`, for the same full-column-set-even-when-empty reason.
  tibble::tibble(
    survey_id = survey_id,
    question_id = purrr::map_chr(items, ~ chr1(.x$id)),
    type = purrr::map_chr(items, ~ chr1(.x$type)),
    stats = purrr::map_chr(items, ~ json1(.x[setdiff(names(.x), c("id", "type"))]))
  )
}

#' Parse a `surveycampaign` list into `raw.survey_campaigns` rows
#' @param survey_id Survey id.
#' @param items Result of `alchemer_fetch_all(client, "survey/{id}/surveycampaign")`.
#' @return A tibble matching `raw.survey_campaigns`' columns.
#' @keywords internal
parse_campaigns <- function(survey_id, items) {
  # See parse_surveys()'s comment: no early-return-tibble() shortcut for an
  # empty `items`, for the same full-column-set-even-when-empty reason.
  tibble::tibble(
    survey_id = survey_id,
    campaign_id = purrr::map_chr(items, ~ chr1(.x$id)),
    payload = purrr::map_chr(items, ~ json1(.x))
  )
}
