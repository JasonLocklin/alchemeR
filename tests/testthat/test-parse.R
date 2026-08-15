load_fixture <- function(name) {
  jsonlite::fromJSON(fixture_path(name), simplifyVector = FALSE)
}

# --- parse_surveys ------------------------------------------------------------

test_that("parse_surveys handles a null statistics block (QuestionLibrary surveys)", {
  items <- load_fixture("survey_list.json")$data
  out <- parse_surveys(items)
  expect_equal(nrow(out), 2)
  expect_equal(out$survey_id, c("8611799", "8611800"))
  expect_true(is.na(out$statistics[2]))
  expect_equal(jsonlite::fromJSON(out$statistics[1])$Complete, 5)
  expect_true(nzchar(out$payload[1]))
})

test_that("parse_surveys never errors on an empty list, and keeps every column", {
  out <- parse_surveys(list())
  expect_equal(nrow(out), 0)
  expect_true("survey_id" %in% names(out))
  expect_true("title" %in% names(out))
})

# --- parse_survey_definition ---------------------------------------------------

test_that("parse_survey_definition flattens pages/questions/options and merges varname", {
  def <- load_fixture("survey_get.json")
  qitems <- load_fixture("surveyquestion_list.json")$data
  varnames <- parse_varnames(qitems)

  out <- parse_survey_definition("8611799", def, varnames)

  expect_equal(nrow(out$pages), 1)
  expect_equal(nrow(out$questions), 3)
  expect_equal(nrow(out$options), 2) # only question 26 (RADIO) has options
  expect_equal(nrow(out$definition), 1)

  expect_equal(out$questions$varname[out$questions$question_id == "2"], "first_name")
  # varname is null in the surveyquestion fixture for question 26
  expect_true(is.na(out$questions$varname[out$questions$question_id == "26"]))

  expect_equal(out$options$question_id, c("26", "26"))
  expect_equal(out$pages$page_order, 1)
  expect_equal(out$questions$question_order, 1:3)

  # payload columns exist for every table matching the raw.* schema, and
  # don't duplicate the child rows that already have their own table/payload
  expect_true(all(nzchar(out$pages$payload)))
  expect_true(all(nzchar(out$options$payload)))
  expect_false("questions" %in% names(jsonlite::fromJSON(out$pages$payload[1])))
  expect_false("options" %in% names(jsonlite::fromJSON(out$questions$payload[out$questions$question_id == "26"])))
})

test_that("parse_survey_definition copes with an unknown question type and empty properties", {
  def <- list(
    id = "1", pages = list(list(
      id = "1", title = list(English = "Page 1"), questions = list(list(
        id = "99", base_type = "Question", type = "SOME_FUTURE_TYPE",
        title = list(English = "Mystery"), properties = list(), options = list()
      ))
    ))
  )
  out <- parse_survey_definition("1", def)
  expect_equal(nrow(out$questions), 1)
  expect_equal(out$questions$type, "SOME_FUTURE_TYPE")
  expect_true(is.na(out$questions$varname))
})

test_that("parse_survey_definition returns empty frames for a survey with no pages", {
  out <- parse_survey_definition("1", list(id = "1"))
  expect_equal(nrow(out$pages), 0)
  expect_equal(nrow(out$questions), 0)
  expect_equal(nrow(out$options), 0)
  expect_equal(nrow(out$definition), 1)
})

# --- parse_responses ------------------------------------------------------------

test_that("parse_responses handles empty vs. populated url_variables/data_quality", {
  items <- load_fixture("surveyresponse_list.json")$data
  out <- parse_responses("8611799", items)
  expect_equal(nrow(out), 2)

  expect_equal(jsonlite::fromJSON(out$url_variables[1]), list())
  expect_equal(jsonlite::fromJSON(out$data_quality[1]), list())
  expect_equal(jsonlite::fromJSON(out$url_variables[2])$utm_source$value, "newsletter")
  expect_true(jsonlite::fromJSON(out$data_quality[2])$opentext.gibberish)
})

test_that("parse_responses preserves answer: null with shown: false verbatim in survey_data", {
  items <- load_fixture("surveyresponse_list.json")$data
  out <- parse_responses("8611799", items)
  sd <- jsonlite::fromJSON(out$survey_data[1], simplifyVector = FALSE)
  expect_null(sd[["82"]]$answer)
  expect_false(sd[["82"]]$shown)
})

test_that("parse_responses redacts PII from payload but keeps everything else", {
  items <- load_fixture("surveyresponse_list.json")$data
  out <- parse_responses("8611799", items)
  payload <- jsonlite::fromJSON(out$payload[1])
  expect_false("ip_address" %in% names(payload))
  expect_false("latitude" %in% names(payload))
  expect_false("longitude" %in% names(payload))
  expect_false("city" %in% names(payload))
  expect_false("user_agent" %in% names(payload))
  expect_true("session_id" %in% names(payload))
  expect_true("survey_data" %in% names(payload))
})

test_that("parse_responses never stores PII in a dedicated column either", {
  items <- load_fixture("surveyresponse_list.json")$data
  out <- parse_responses("8611799", items)
  expect_false(any(c("ip_address", "latitude", "longitude", "user_agent") %in% names(out)))
})

test_that("parse_responses handles is_test_data as a string and preserves it verbatim", {
  items <- load_fixture("surveyresponse_list.json")$data
  out <- parse_responses("8611799", items)
  expect_equal(out$is_test_data, c("0", "1"))
})

test_that("parse_responses never errors on an empty list, and keeps every column", {
  out <- parse_responses("1", list())
  expect_equal(nrow(out), 0)
  expect_true("response_id" %in% names(out))
  expect_true("survey_data" %in% names(out))
})

# --- parse_statistics -----------------------------------------------------------

test_that("parse_statistics survives the 'total responses' space-in-key gotcha", {
  items <- load_fixture("surveystatistic_list.json")$data
  out <- parse_statistics("8611799", items)
  expect_equal(nrow(out), 3)
  stats26 <- jsonlite::fromJSON(out$stats[out$question_id == "26"])
  expect_equal(stats26[["total responses"]], "3")
  expect_equal(nrow(stats26$breakdown), 2)
})

test_that("parse_statistics handles a non-answerable type with only id/type", {
  items <- load_fixture("surveystatistic_list.json")$data
  out <- parse_statistics("8611799", items)
  stats2 <- jsonlite::fromJSON(out$stats[out$question_id == "2"])
  expect_equal(length(stats2), 0)
})

# --- parse_campaigns -------------------------------------------------------------

test_that("parse_campaigns keeps the full item verbatim in payload", {
  items <- load_fixture("surveycampaign_list.json")$data
  out <- parse_campaigns("8611799", items)
  expect_equal(nrow(out), 1)
  expect_equal(out$campaign_id, "300865")
  expect_equal(jsonlite::fromJSON(out$payload)$name, "Default Link")
})

# --- chr1 ------------------------------------------------------------------------

test_that("chr1 returns exactly one string whatever shape the field arrives in", {
  # Regression test: chr1() was a bare as.character(), which assumed every
  # field Alchemer documents as a scalar always arrives as one. It does not.
  # purrr::map_chr() then aborted the whole survey's refresh with "Result must
  # be length 1, not 2" -- a hard ingest failure caused purely by field shape.
  expect_equal(chr1("8611799"), "8611799")
  expect_equal(chr1(6), "6")
  expect_equal(chr1(TRUE), "TRUE")

  # Absent, null, and empty all mean "nothing to record".
  expect_true(is.na(chr1(NULL)))
  expect_true(is.na(chr1(list())))
  expect_true(is.na(chr1(character(0))))

  # An array where a scalar was expected: kept as JSON, not flattened into two
  # values and not dropped.
  expect_equal(chr1(list("1", "2")), '["1","2"]')
  expect_equal(chr1(c("1", "2")), '["1","2"]')

  # An object where a scalar was expected. This is the case that did not
  # error: as.character() deparsed it, and `raw` archived an R expression.
  expect_equal(chr1(list(id = "1", name = "Research")), '{"id":"1","name":"Research"}')
  expect_false(grepl("list(", chr1(list(id = "1", name = "Research")), fixed = TRUE))

  # Whatever the shape, the contract is one string.
  shapes <- list(
    "a", 1L, list("a", "b"), list(id = "1"), list(list(id = "1"), list(id = "2")), NULL, list()
  )
  expect_true(all(vapply(shapes, function(x) length(chr1(x)), integer(1)) == 1L))
})

test_that("parse_surveys survives a survey shared across two teams", {
  # `team` is a single id on most surveys and an array on a survey shared
  # between teams -- documented as a scalar on the list endpoint and as an
  # array of {id, name} on the detail endpoint. Either must ingest.
  items <- list(
    list(id = "1", title = "One", type = "Standard Survey", status = "Launched",
         created_on = "2026-01-01 00:00:00", modified_on = "2026-01-01 00:00:00",
         team = "1"),
    list(id = "2", title = "Two", type = "Standard Survey", status = "Launched",
         created_on = "2026-01-01 00:00:00", modified_on = "2026-01-01 00:00:00",
         team = list("1", "2"))
  )
  out <- parse_surveys(items)
  expect_equal(nrow(out), 2)
  expect_equal(out$team, c("1", '["1","2"]'))
  # And the verbatim copy still holds both ids.
  expect_equal(jsonlite::fromJSON(out$payload[2])$team, c("1", "2"))
})

test_that("parse_responses survives an array where a scalar field is expected", {
  items <- list(
    mock_response("r1"),
    modifyList(mock_response("r2"), list(language = list("English", "French")))
  )
  out <- parse_responses("1", items)
  expect_equal(nrow(out), 2)
  expect_equal(out$language, c("English", '["English","French"]'))
})

test_that("parse_survey_definition keeps a structured value out of a scalar column", {
  def <- list(id = "1", pages = list(list(
    id = "1", description = "", questions = list(list(
      id = "2", base_type = "Question", type = "TEXTBOX",
      # Alchemer returns shortname as a string; a survey that returns a
      # structure here must not deparse into raw as "list(...)".
      shortname = list(en = "First name"),
      options = list()
    ))
  )))
  out <- parse_survey_definition("1", def)
  expect_equal(out$questions$shortname, '{"en":"First name"}')
})
