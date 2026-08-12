# A fixture-backed mock router for ingest()-level integration tests
# (ADR-010): routes by path/query instead of a strict call-order queue, so a
# single `state` describes an entire account's worth of surveys/responses
# and tests can mutate it between successive ingest() calls to simulate a
# survey changing, a response vanishing, and so on.
#
# `state$surveys` is a list of survey-list items (id, title, modified_on, ...).
# `state$responses` is a named list, survey_id -> list of response items.
# `state$definitions` is a named list, survey_id -> definition tree (falls
# back to a minimal single-page/no-questions tree if absent).

survey_id_from_path <- function(path) {
  pattern <- "survey/([^/]+)"
  m <- regmatches(path, regexpr(pattern, path))
  if (length(m) == 0 || !nzchar(m)) {
    return(NA_character_)
  }
  sub(pattern, "\\1", m)
}

minimal_definition <- function(survey_id) {
  list(id = survey_id, pages = list())
}

mock_client_router <- function(state) {
  function(req) {
    parsed <- httr2::url_parse(req$url)
    path <- parsed$path
    q <- parsed$query %||% list()
    survey_id <- survey_id_from_path(path)

    body <- if (grepl("/surveyresponse$", path) && !is.null(q$order_by)) {
      # probe: resultsperpage=1&order_by=-date_updated
      items <- state$responses[[survey_id]] %||% list()
      newest <- if (length(items) > 0) list(items[[length(items)]]) else list()
      list(result_ok = TRUE, total_count = length(items), page = 1, total_pages = 1, data = newest)
    } else if (grepl("/surveyresponse$", path)) {
      items <- state$responses[[survey_id]] %||% list()
      list(result_ok = TRUE, total_count = length(items), page = 1, total_pages = 1, data = items)
    } else if (grepl("/surveyquestion$", path)) {
      list(result_ok = TRUE, total_count = 0, page = 1, total_pages = 1, data = list())
    } else if (grepl("/surveycampaign$", path)) {
      list(result_ok = TRUE, total_count = 0, page = 1, total_pages = 1, data = list())
    } else if (grepl("/surveystatistic$", path)) {
      list(result_ok = TRUE, total_count = 0, page = 1, total_pages = 1, data = list())
    } else if (grepl("/survey/[^/]+$", path)) {
      state$definitions[[survey_id]] %||% minimal_definition(survey_id)
    } else if (grepl("/survey$", path)) {
      list(
        result_ok = TRUE, total_count = length(state$surveys), page = 1, total_pages = 1,
        data = state$surveys
      )
    } else {
      list(result_ok = FALSE, code = 404, message = "not found in mock router")
    }

    httr2::response_json(status_code = 200, body = body)
  }
}

# A minimal survey-list item.
mock_survey <- function(id, title = "Survey", modified_on = "2026-01-01 00:00:00") {
  list(id = id, title = title, type = "Standard Survey", status = "Launched",
       created_on = modified_on, modified_on = modified_on, team = "1")
}

# A minimal response item.
mock_response <- function(id, date_updated = "2026-01-01 00:00:01") {
  list(id = id, status = "Complete", is_test_data = "0",
       date_submitted = "2026-01-01 00:00:00 EST", date_updated = date_updated,
       date_started = "2026-01-01 00:00:00 EST", session_id = paste0("s", id),
       language = "English", link_id = "1", contact_id = "", response_time = 1,
       url_variables = list(), data_quality = list(), survey_data = list())
}
