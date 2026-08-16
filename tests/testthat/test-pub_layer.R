seed_survey <- function(con, survey_id = "1", title = "Q1 Customer Feedback") {
  qs <- function(x) DBI::dbQuoteString(con, x)
  DBI::dbExecute(con, glue::glue(
    "INSERT INTO alchemer.raw.surveys (survey_id, title, type, status, created_on, modified_on, team, is_deleted)
     VALUES ({qs(survey_id)}, {qs(title)}, 'Standard Survey', 'Launched',
             '2026-01-01 00:00:00', '2026-01-01 00:00:00', '1', FALSE)"
  ))
  DBI::dbExecute(con, glue::glue(
    "INSERT INTO alchemer.raw.survey_questions
       (survey_id, question_id, base_type, type, title, shortname, question_order)
     VALUES
       ({qs(survey_id)}, '2', 'Question', 'TEXTBOX', '{{\"English\":\"First Name\"}}', 'First Name', 1),
       ({qs(survey_id)}, '26', 'Question', 'RADIO', '{{\"English\":\"How satisfied are you?\"}}', 'Satisfaction', 2)"
  ))
  DBI::dbExecute(con, glue::glue(
    "INSERT INTO alchemer.raw.survey_question_options
       (survey_id, question_id, option_id, title, value, option_order)
     VALUES
       ({qs(survey_id)}, '26', '10001', '{{\"English\":\"Satisfied\"}}', 'Satisfied', 1),
       ({qs(survey_id)}, '26', '10002', '{{\"English\":\"Unsatisfied\"}}', 'Unsatisfied', 2)"
  ))
  survey_data <- function(first_name, satisfaction) {
    jsonlite::toJSON(list(
      `2` = list(id = 2, answer = first_name, shown = TRUE),
      `26` = list(id = 26, answer = satisfaction, shown = TRUE)
    ), auto_unbox = TRUE)
  }
  DBI::dbExecute(con, glue::glue(
    "INSERT INTO alchemer.raw.responses
       (survey_id, response_id, status, is_test_data, date_submitted, date_updated, survey_data, is_deleted)
     VALUES
       ({qs(survey_id)}, 'r1', 'Complete', '0', '2026-01-01 10:00:00 EST', '2026-01-01 10:00:05',
        {qs(survey_data('Ian', 'Satisfied'))}, FALSE),
       ({qs(survey_id)}, 'r2', 'Complete', '0', '2026-01-01 11:00:00 EST', '2026-01-01 11:00:05',
        {qs(survey_data('Ana', 'Unsatisfied'))}, FALSE)"
  ))
}

test_that("pub_layer builds typed surveys/questions/options/responses/answers", {
  dir <- withr::local_tempdir()
  con <- alchemer_db(dir)
  seed_survey(con)
  DBI::dbDisconnect(con, shutdown = TRUE)

  pub_layer(dir)

  con2 <- alchemer_db(dir, read_only = TRUE)
  on.exit(DBI::dbDisconnect(con2, shutdown = TRUE))

  surveys <- DBI::dbGetQuery(con2, "SELECT * FROM alchemer.pub.surveys")
  expect_equal(surveys$title, "Q1 Customer Feedback")
  expect_true(inherits(surveys$created_on, c("POSIXct", "POSIXt")) || is.character(surveys$created_on))

  responses <- DBI::dbGetQuery(con2, "SELECT * FROM alchemer.pub.responses ORDER BY response_id")
  expect_equal(nrow(responses), 2)
  expect_type(responses$is_test_data, "logical")
  expect_false(any(responses$is_test_data))

  answers <- DBI::dbGetQuery(con2, "SELECT * FROM alchemer.pub.answers")
  expect_equal(nrow(answers), 4) # 2 responses x 2 questions
})

test_that("date_submitted/date_started parse their EST/EDT suffix into the configured timezone", {
  # EST/EDT *are* Toronto's own standard/daylight offsets, so with
  # tz = America/Toronto a properly parsed EST/EDT-suffixed timestamp lands
  # at the same wall-clock value it arrived as -- winter (EST) and summer
  # (EDT) both checked, since a naive fixed-offset assumption would get one
  # of the two wrong.
  dir <- withr::local_tempdir()
  con <- alchemer_db(dir)
  DBI::dbExecute(con, "INSERT INTO alchemer.raw.surveys (survey_id, title, is_deleted) VALUES ('1', 'S', FALSE)")
  DBI::dbExecute(con, "INSERT INTO alchemer.raw.responses
    (survey_id, response_id, date_submitted, date_started, is_deleted) VALUES
    ('1', 'winter', '2026-01-15 14:30:00 EST', '2026-01-15 14:25:00 EST', FALSE),
    ('1', 'summer', '2026-07-15 14:30:00 EDT', '2026-07-15 14:25:00 EDT', FALSE)")
  DBI::dbDisconnect(con, shutdown = TRUE)

  withr::local_envvar(ALCHEMER_TZ = "America/Toronto")
  pub_layer(dir)

  con2 <- alchemer_db(dir, read_only = TRUE)
  on.exit(DBI::dbDisconnect(con2, shutdown = TRUE))
  responses <- DBI::dbGetQuery(con2, "
    SELECT response_id, date_submitted, date_started FROM alchemer.pub.responses ORDER BY response_id")
  expect_equal(format(responses$date_submitted[responses$response_id == "winter"]), "2026-01-15 14:30:00")
  expect_equal(format(responses$date_submitted[responses$response_id == "summer"]), "2026-07-15 14:30:00")
  expect_equal(format(responses$date_started[responses$response_id == "winter"]), "2026-01-15 14:25:00")
})

test_that("a non-Eastern tz renders the same instant in that zone", {
  # The suffix fixes the instant; tz only decides how it is displayed. UTC is
  # 5 hours ahead of EST, so 14:30 EST must read 19:30 -- proof that tz is
  # genuinely applied rather than the suffix being string-stripped.
  dir <- withr::local_tempdir()
  con <- alchemer_db(dir)
  DBI::dbExecute(con, "INSERT INTO alchemer.raw.surveys (survey_id, title, is_deleted) VALUES ('1', 'S', FALSE)")
  DBI::dbExecute(con, "INSERT INTO alchemer.raw.responses
    (survey_id, response_id, date_submitted, is_deleted) VALUES
    ('1', 'r1', '2026-01-15 14:30:00 EST', FALSE)")
  DBI::dbDisconnect(con, shutdown = TRUE)

  withr::local_envvar(ALCHEMER_TZ = "UTC")
  pub_layer(dir)

  con2 <- alchemer_db(dir, read_only = TRUE)
  on.exit(DBI::dbDisconnect(con2, shutdown = TRUE))
  submitted <- DBI::dbGetQuery(con2, "SELECT date_submitted FROM alchemer.pub.responses")$date_submitted
  expect_equal(format(submitted), "2026-01-15 19:30:00")
})

test_that("date_updated/created_on/modified_on are kept as the account-local time Alchemer sent", {
  # Regression test (code review): these three carry no timezone suffix and
  # were read as UTC, then shifted into the configured zone -- which moved
  # every one of them 4-5 hours earlier, into the previous evening, and made
  # date_updated land *before* date_started for every response. Alchemer's own
  # documented example (see inst/extdata/fixtures/surveyresponse_list.json)
  # shows date_updated 6 seconds after an EST-suffixed date_submitted, on the
  # same clock: they are already account-local, so there is nothing to shift.
  dir <- withr::local_tempdir()
  con <- alchemer_db(dir)
  DBI::dbExecute(con, "INSERT INTO alchemer.raw.surveys
    (survey_id, title, created_on, modified_on, is_deleted) VALUES
    ('1', 'S', '2026-01-15 19:00:00', '2026-07-15 18:00:00', FALSE)")
  DBI::dbExecute(con, "INSERT INTO alchemer.raw.responses
    (survey_id, response_id, date_started, date_submitted, date_updated, is_deleted) VALUES
    ('1', 'r1', '2026-01-15 18:59:50 EST', '2026-01-15 18:59:57 EST', '2026-01-15 19:00:00', FALSE)")
  DBI::dbDisconnect(con, shutdown = TRUE)

  withr::local_envvar(ALCHEMER_TZ = "America/Toronto")
  pub_layer(dir)

  con2 <- alchemer_db(dir, read_only = TRUE)
  on.exit(DBI::dbDisconnect(con2, shutdown = TRUE))
  surveys <- DBI::dbGetQuery(con2, "SELECT created_on, modified_on FROM alchemer.pub.surveys")
  expect_equal(format(surveys$created_on), "2026-01-15 19:00:00")
  expect_equal(format(surveys$modified_on), "2026-07-15 18:00:00")

  responses <- DBI::dbGetQuery(con2, "
    SELECT date_started, date_submitted, date_updated FROM alchemer.pub.responses")
  expect_equal(format(responses$date_updated), "2026-01-15 19:00:00")
  # The property that was violated before: a response cannot be updated
  # before it was started or submitted.
  expect_gt(responses$date_updated, responses$date_started)
  expect_gt(responses$date_updated, responses$date_submitted)
})

test_that("raw timestamps stay exactly as Alchemer sent them, whatever tz is used", {
  # ADR-003: the archive is verbatim. pub_layer() must never write back to raw.
  dir <- withr::local_tempdir()
  con <- alchemer_db(dir)
  DBI::dbExecute(con, "INSERT INTO alchemer.raw.surveys (survey_id, title, is_deleted) VALUES ('1', 'S', FALSE)")
  DBI::dbExecute(con, "INSERT INTO alchemer.raw.responses
    (survey_id, response_id, date_submitted, date_updated, is_deleted) VALUES
    ('1', 'r1', '2026-01-15 14:30:00 EST', '2026-01-15 14:30:05', FALSE)")
  DBI::dbDisconnect(con, shutdown = TRUE)

  withr::local_envvar(ALCHEMER_TZ = "Australia/Sydney")
  pub_layer(dir)

  con2 <- alchemer_db(dir, read_only = TRUE)
  on.exit(DBI::dbDisconnect(con2, shutdown = TRUE))
  raw <- DBI::dbGetQuery(con2, "SELECT date_submitted, date_updated FROM alchemer.raw.responses")
  expect_equal(raw$date_submitted, "2026-01-15 14:30:00 EST")
  expect_equal(raw$date_updated, "2026-01-15 14:30:05")
})

test_that("pub_layer() rejects a timezone that isn't an IANA name", {
  dir <- withr::local_tempdir()
  withr::local_envvar(ALCHEMER_TZ = "Eastern")
  expect_error(pub_layer(dir), class = "alchemeR_config_error")
})

test_that("INSERT ... BY NAME rejects a column-name mismatch instead of silently misrouting", {
  # Regression test (ultrareview): pub_table_columns existed as the same
  # column-order-safety mechanism used for raw_table_columns/
  # meta_table_columns, but pub_layer.R's hand-written INSERT ... SELECT
  # statements never referenced it -- dead code with no safety net behind
  # the parallel hand-maintained SELECT lists. Every INSERT in pub_layer.R
  # now uses `BY NAME`, which matches SELECT output to target columns by
  # name rather than position; this locks in that DuckDB actually enforces
  # that (a typo'd alias fails loudly) rather than silently reordering.
  dir <- withr::local_tempdir()
  con <- alchemer_db(dir)
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))

  err <- tryCatch(
    DBI::dbExecute(con, "
      INSERT INTO alchemer.pub.surveys BY NAME
      SELECT '1' AS survey_id, 'Typo' AS ttile"),
    error = function(e) e
  )
  expect_s3_class(err, "error")
  expect_match(conditionMessage(err), "ttile|column")
})

test_that("pub_layer() rejects a language that could break the generated SQL", {
  # Regression test (ultrareview): title_sql() interpolated the language
  # directly into a SQL/JSONPath string with no escaping. It is validated at
  # the point it is read now (ALCHEMER_LANGUAGE), but the property worth
  # testing is unchanged: a value like this must never reach the query.
  dir <- withr::local_tempdir()
  con <- alchemer_db(dir)
  seed_survey(con)
  DBI::dbDisconnect(con, shutdown = TRUE)

  withr::local_envvar(ALCHEMER_LANGUAGE = "English\") --")
  expect_error(pub_layer(dir), class = "alchemeR_config_error")
  withr::local_envvar(ALCHEMER_LANGUAGE = "en'glish")
  expect_error(pub_layer(dir), class = "alchemeR_config_error")
})

test_that("a survey_id containing a quote character doesn't break wide-view management", {
  # Regression test (ultrareview): drop_existing_wide_views() interpolated
  # survey_id into a LIKE pattern with no SQL quoting at all, unlike every
  # other use of survey_id in this file.
  dir <- withr::local_tempdir()
  con <- alchemer_db(dir)
  seed_survey(con, survey_id = "1'2", title = "Odd Id Survey")
  DBI::dbDisconnect(con, shutdown = TRUE)

  pub_layer(dir) # first run: creates the view
  pub_layer(dir) # second run: must find and drop it again without erroring

  con2 <- alchemer_db(dir, read_only = TRUE)
  on.exit(DBI::dbDisconnect(con2, shutdown = TRUE))
  views <- DBI::dbGetQuery(con2, "
    SELECT table_name FROM information_schema.tables
    WHERE table_catalog = 'alchemer' AND table_schema = 'pub' AND table_name LIKE 'wide\\_%' ESCAPE '\\'")$table_name
  expect_equal(length(views), 1)
})

test_that("pub.answers reconciles against raw.responses.survey_data", {
  dir <- withr::local_tempdir()
  con <- alchemer_db(dir)
  seed_survey(con)
  n_survey_data_keys <- DBI::dbGetQuery(con, "
    SELECT SUM(len) AS n FROM (
      SELECT list_count(json_keys(survey_data)) AS len FROM alchemer.raw.responses
    )")$n
  DBI::dbDisconnect(con, shutdown = TRUE)

  pub_layer(dir)

  con2 <- alchemer_db(dir, read_only = TRUE)
  on.exit(DBI::dbDisconnect(con2, shutdown = TRUE))
  n_answers <- DBI::dbGetQuery(con2, "SELECT COUNT(*) n FROM alchemer.pub.answers")$n
  expect_equal(n_answers, n_survey_data_keys)
})

test_that("pub.answers resolves reporting_value and option_id for coded answers", {
  dir <- withr::local_tempdir()
  con <- alchemer_db(dir)
  seed_survey(con)
  DBI::dbDisconnect(con, shutdown = TRUE)

  pub_layer(dir)

  con2 <- alchemer_db(dir, read_only = TRUE)
  on.exit(DBI::dbDisconnect(con2, shutdown = TRUE))
  answers <- DBI::dbGetQuery(con2, "
    SELECT * FROM alchemer.pub.answers WHERE question_id = '26' ORDER BY response_id")
  expect_equal(answers$option_id, c("10001", "10002"))
  expect_equal(answers$reporting_value, c("Satisfied", "Unsatisfied"))
})

test_that("the wide view round-trips to the same data as pub.answers (long)", {
  dir <- withr::local_tempdir()
  con <- alchemer_db(dir)
  seed_survey(con)
  DBI::dbDisconnect(con, shutdown = TRUE)

  pub_layer(dir)

  con2 <- alchemer_db(dir, read_only = TRUE)
  on.exit(DBI::dbDisconnect(con2, shutdown = TRUE))
  wide <- DBI::dbGetQuery(con2, "
    SELECT * FROM alchemer.pub.wide_q1_customer_feedback_1 ORDER BY response_id")
  expect_equal(wide$response_id, c("r1", "r2"))
  expect_equal(wide$first_name, c("Ian", "Ana"))
  expect_equal(wide$satisfaction, c("Satisfied", "Unsatisfied"))
})

test_that("pub_layer regenerates the wide view when a question is added", {
  dir <- withr::local_tempdir()
  con <- alchemer_db(dir)
  seed_survey(con)
  DBI::dbDisconnect(con, shutdown = TRUE)
  pub_layer(dir)

  con <- alchemer_db(dir)
  DBI::dbExecute(con, "
    INSERT INTO alchemer.raw.survey_questions
      (survey_id, question_id, base_type, type, title, shortname, question_order)
    VALUES ('1', '99', 'Question', 'TEXTBOX', '{\"English\":\"Last Name\"}', 'Last Name', 3)")
  new_answer <- jsonlite::toJSON(list(`99` = list(id = 99, answer = "Smith", shown = TRUE)), auto_unbox = TRUE)
  DBI::dbExecute(con, glue::glue("
    UPDATE alchemer.raw.responses SET survey_data = json_merge_patch(survey_data, {DBI::dbQuoteString(con, new_answer)})
    WHERE response_id = 'r1'"))
  DBI::dbDisconnect(con, shutdown = TRUE)

  pub_layer(dir)

  con2 <- alchemer_db(dir, read_only = TRUE)
  on.exit(DBI::dbDisconnect(con2, shutdown = TRUE))
  wide <- DBI::dbGetQuery(con2, "
    SELECT * FROM alchemer.pub.wide_q1_customer_feedback_1 ORDER BY response_id")
  expect_true("last_name" %in% names(wide))
  expect_equal(wide$last_name[wide$response_id == "r1"], "Smith")
})

test_that("pub_layer drops a survey's stale wide view when its title changes", {
  dir <- withr::local_tempdir()
  con <- alchemer_db(dir)
  seed_survey(con)
  DBI::dbDisconnect(con, shutdown = TRUE)
  pub_layer(dir)

  con <- alchemer_db(dir)
  DBI::dbExecute(con, "UPDATE alchemer.raw.surveys SET title = 'Renamed Survey' WHERE survey_id = '1'")
  DBI::dbDisconnect(con, shutdown = TRUE)
  pub_layer(dir)

  con2 <- alchemer_db(dir, read_only = TRUE)
  on.exit(DBI::dbDisconnect(con2, shutdown = TRUE))
  views <- DBI::dbGetQuery(con2, "
    SELECT table_name FROM information_schema.tables
    WHERE table_catalog = 'alchemer' AND table_schema = 'pub'
      AND table_name LIKE 'wide\\_%\\_1' ESCAPE '\\'")$table_name
  expect_equal(views, "wide_renamed_survey_1")
})

test_that("test and deleted responses are flagged, not dropped, in pub.responses/answers", {
  dir <- withr::local_tempdir()
  con <- alchemer_db(dir)
  seed_survey(con)
  DBI::dbExecute(con, "UPDATE alchemer.raw.responses SET is_test_data = '1' WHERE response_id = 'r1'")
  DBI::dbExecute(con, "
    UPDATE alchemer.raw.responses SET is_deleted = TRUE, deleted_detected_at = now()
    WHERE response_id = 'r2'")
  DBI::dbDisconnect(con, shutdown = TRUE)

  pub_layer(dir)

  con2 <- alchemer_db(dir, read_only = TRUE)
  on.exit(DBI::dbDisconnect(con2, shutdown = TRUE))
  responses <- DBI::dbGetQuery(con2, "SELECT * FROM alchemer.pub.responses ORDER BY response_id")
  expect_equal(nrow(responses), 2) # both still present
  expect_true(responses$is_test_data[responses$response_id == "r1"])
  expect_true(responses$is_deleted[responses$response_id == "r2"])
  answers <- DBI::dbGetQuery(con2, "SELECT is_deleted FROM alchemer.pub.answers WHERE response_id = 'r2'")
  expect_true(all(answers$is_deleted))
})

test_that("survey_wide() pivots directly from raw for a survey not run through pub_layer()", {
  dir <- withr::local_tempdir()
  con <- alchemer_db(dir)
  seed_survey(con)

  out <- survey_wide(con, "1")
  DBI::dbDisconnect(con, shutdown = TRUE)

  expect_equal(nrow(out), 2)
  expect_true(all(c("first_name", "satisfaction") %in% names(out)))
})

test_that("pub_layer(surveys = ) rebuilds only the requested survey", {
  dir <- withr::local_tempdir()
  con <- alchemer_db(dir)
  seed_survey(con, "1", "Survey One")
  seed_survey(con, "2", "Survey Two")
  DBI::dbDisconnect(con, shutdown = TRUE)

  pub_layer(dir, surveys = "1")

  con2 <- alchemer_db(dir, read_only = TRUE)
  on.exit(DBI::dbDisconnect(con2, shutdown = TRUE))
  expect_equal(DBI::dbGetQuery(con2, "SELECT COUNT(*) n FROM alchemer.pub.surveys")$n, 1)
  expect_equal(DBI::dbGetQuery(con2, "SELECT survey_id FROM alchemer.pub.surveys")$survey_id, "1")
})

# --- Incremental rebuilds (ADR-017) -----------------------------------------

# What ingest()'s merge does to a row it actually changed: stamps ingested_at.
# The tests below edit `raw` directly, so they have to do it themselves.
stamp_raw <- function(con, table, at = Sys.time()) {
  DBI::dbExecute(con, glue::glue(
    "UPDATE alchemer.raw.{table} SET ingested_at = {DBI::dbQuoteLiteral(con, at)}"
  ))
}

a_build <- function(...) {
  modifyList(list(language = "English", tz = "America/Toronto", wide_views = TRUE), list(...))
}
a_prior <- function(...) {
  as.data.frame(modifyList(
    list(survey_id = "1", source_watermark = "w", language = "English", tz = "America/Toronto",
         wide_views = TRUE),
    list(...)
  ), stringsAsFactors = FALSE)
}
a_watermark <- function(w = "w") data.frame(survey_id = "1", source_watermark = w)

test_that("decide_rebuild: a survey that has never been built is rebuilt", {
  out <- decide_rebuild("1", a_watermark(), a_prior(survey_id = "other"), a_build())
  expect_true(out$rebuild)
  expect_match(out$reason, "never built")
})

test_that("decide_rebuild: an unchanged survey built the same way is skipped", {
  out <- decide_rebuild("1", a_watermark(), a_prior(), a_build())
  expect_false(out$rebuild)
  expect_equal(out$reason, "unchanged")
})

test_that("decide_rebuild: a moved watermark rebuilds", {
  out <- decide_rebuild("1", a_watermark("w2"), a_prior(), a_build())
  expect_true(out$rebuild)
  expect_match(out$reason, "raw data changed")
})

test_that("decide_rebuild: each build setting rebuilds when it changes", {
  expect_match(
    decide_rebuild("1", a_watermark(), a_prior(), a_build(language = "French"))$reason, "language"
  )
  expect_match(decide_rebuild("1", a_watermark(), a_prior(), a_build(tz = "UTC"))$reason, "tz")
  expect_match(
    decide_rebuild("1", a_watermark(), a_prior(), a_build(wide_views = FALSE))$reason, "wide_views"
  )
})

test_that("decide_rebuild: an unknown watermark is never treated as unchanged", {
  # A survey_id the caller passed that isn't in raw.surveys at all. There is
  # nothing to compare it against, so it must fall to the rebuild side.
  out <- decide_rebuild("1", a_watermark(NA_character_), a_prior(), a_build())
  expect_true(out$rebuild)
})

test_that("pub_layer() rebuilds nothing on a second run with nothing changed", {
  # The property the incremental build exists for: a closed survey with no new
  # activity costs nothing on every subsequent pipeline tick.
  dir <- withr::local_tempdir()
  con <- alchemer_db(dir)
  seed_survey(con)
  DBI::dbDisconnect(con, shutdown = TRUE)

  expect_equal(pub_layer(dir), "1")
  expect_equal(pub_layer(dir), character(0))

  # ...and skipping did not cost the data: pub is still complete.
  con2 <- alchemer_db(dir, read_only = TRUE)
  on.exit(DBI::dbDisconnect(con2, shutdown = TRUE))
  expect_equal(DBI::dbGetQuery(con2, "SELECT COUNT(*) n FROM alchemer.pub.answers")$n, 4)
})

test_that("pub_layer() rebuilds the changed survey and leaves its neighbour alone", {
  dir <- withr::local_tempdir()
  con <- alchemer_db(dir)
  seed_survey(con, "1", "Survey One")
  seed_survey(con, "2", "Survey Two")
  DBI::dbDisconnect(con, shutdown = TRUE)
  pub_layer(dir)

  con <- alchemer_db(dir)
  DBI::dbExecute(con, "
    INSERT INTO alchemer.raw.responses
      (survey_id, response_id, status, is_test_data, date_submitted, date_updated,
       survey_data, is_deleted, ingested_at)
    VALUES ('2', 'r3', 'Complete', '0', '2026-02-01 10:00:00 EST', '2026-02-01 10:00:05',
            '{\"2\": {\"answer\": \"Zoe\", \"shown\": true}}', FALSE, now())")
  DBI::dbDisconnect(con, shutdown = TRUE)

  expect_equal(pub_layer(dir), "2")

  con2 <- alchemer_db(dir, read_only = TRUE)
  on.exit(DBI::dbDisconnect(con2, shutdown = TRUE))
  expect_equal(
    DBI::dbGetQuery(con2, "SELECT COUNT(*) n FROM alchemer.pub.responses WHERE survey_id = '2'")$n, 3
  )
})

test_that("pub_layer() detects a deletion that never touches ingested_at", {
  # The case max(ingested_at) alone cannot see: flagging a response deleted
  # sets is_deleted/deleted_detected_at and leaves ingested_at where it was.
  # Without deleted_detected_at in the watermark this survey would be skipped
  # forever, and pub.responses would go on saying the response is live.
  dir <- withr::local_tempdir()
  con <- alchemer_db(dir)
  seed_survey(con)
  stamp_raw(con, "responses")
  DBI::dbDisconnect(con, shutdown = TRUE)
  pub_layer(dir)

  con <- alchemer_db(dir)
  DBI::dbExecute(con, "
    UPDATE alchemer.raw.responses SET is_deleted = TRUE, deleted_detected_at = now()
    WHERE survey_id = '1' AND response_id = 'r2'")
  DBI::dbDisconnect(con, shutdown = TRUE)

  expect_equal(pub_layer(dir), "1")

  con2 <- alchemer_db(dir, read_only = TRUE)
  on.exit(DBI::dbDisconnect(con2, shutdown = TRUE))
  expect_true(DBI::dbGetQuery(con2, "
    SELECT is_deleted FROM alchemer.pub.responses WHERE response_id = 'r2'")$is_deleted)
})

test_that("pub_layer() detects a row removed outright", {
  # The count(*) signal: question and option rows that vanish upstream are
  # deleted rather than flagged, so neither timestamp moves.
  dir <- withr::local_tempdir()
  con <- alchemer_db(dir)
  seed_survey(con)
  stamp_raw(con, "survey_question_options")
  DBI::dbDisconnect(con, shutdown = TRUE)
  pub_layer(dir)

  con <- alchemer_db(dir)
  DBI::dbExecute(con, "DELETE FROM alchemer.raw.survey_question_options WHERE option_id = '10002'")
  DBI::dbDisconnect(con, shutdown = TRUE)

  expect_equal(pub_layer(dir), "1")

  con2 <- alchemer_db(dir, read_only = TRUE)
  on.exit(DBI::dbDisconnect(con2, shutdown = TRUE))
  expect_equal(DBI::dbGetQuery(con2, "SELECT COUNT(*) n FROM alchemer.pub.options")$n, 1)
})

test_that("pub_layer() rebuilds when tz changes, so pub can't hold two timezones at once", {
  dir <- withr::local_tempdir()
  con <- alchemer_db(dir)
  seed_survey(con)
  DBI::dbDisconnect(con, shutdown = TRUE)
  withr::local_envvar(ALCHEMER_TZ = "America/Toronto")
  pub_layer(dir)

  withr::local_envvar(ALCHEMER_TZ = "UTC")
  expect_equal(pub_layer(dir), "1")

  con2 <- alchemer_db(dir, read_only = TRUE)
  on.exit(DBI::dbDisconnect(con2, shutdown = TRUE))
  submitted <- DBI::dbGetQuery(con2, "
    SELECT date_submitted FROM alchemer.pub.responses WHERE response_id = 'r1'")$date_submitted
  expect_equal(format(submitted, "%H:%M", tz = "UTC"), "15:00")
})

test_that("pub_layer(force = TRUE) rebuilds a survey that has not changed", {
  dir <- withr::local_tempdir()
  con <- alchemer_db(dir)
  seed_survey(con)
  DBI::dbDisconnect(con, shutdown = TRUE)
  pub_layer(dir)

  expect_equal(pub_layer(dir), character(0))
  expect_equal(pub_layer(dir, force = TRUE), "1")
})

test_that("a rebuild that fails partway records no build state and leaves pub as it was", {
  # The ordering that can actually corrupt the result: if meta.pub_state were
  # written outside the survey's transaction, a failure here would mark the
  # survey built from data that had just been rolled back, and every later run
  # would then skip it -- stale, silently, and forever.
  #
  # The failure is real rather than injected: a timezone DuckDB doesn't know
  # reaches SQL in the *third* of the survey's statements, so pub.questions and
  # pub.options have already been written when it fires. Both must go back.
  # (pub_layer() validates tz up front, which is why this calls the internal.)
  dir <- withr::local_tempdir()
  con <- alchemer_db(dir)
  seed_survey(con)
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))

  build <- list(language = "English", tz = "Not/AZone", wide_views = TRUE)
  expect_error(
    rebuild_survey_atomically(con, "1", "T", "English", "Not/AZone", TRUE, "w", build),
    "TimeZone"
  )

  expect_equal(DBI::dbGetQuery(con, "SELECT COUNT(*) n FROM alchemer.pub.questions")$n, 0)
  expect_equal(DBI::dbGetQuery(con, "SELECT COUNT(*) n FROM alchemer.meta.pub_state")$n, 0)
  # The rollback left the connection usable, not stuck in a failed transaction.
  expect_equal(DBI::dbGetQuery(con, "SELECT 1 AS ok")$ok, 1)
})

test_that("a schema minor bump rebuilds pub from scratch and clears the drift", {
  # What the minor version is for (ADR-018): `pub` is derived, so a change to
  # its tables or the SQL that fills them is fixed by throwing the layer away
  # and rebuilding it -- including structure that CREATE TABLE IF NOT EXISTS
  # would otherwise leave in place forever.
  dir <- withr::local_tempdir()
  con <- alchemer_db(dir)
  seed_survey(con)
  DBI::dbDisconnect(con, shutdown = TRUE)
  pub_layer(dir)

  con <- alchemer_db(dir)
  # Drift of both kinds: a stale column on a pub table, and a whole table left
  # behind by a version that no longer declares it.
  DBI::dbExecute(con, "ALTER TABLE alchemer.pub.answers ADD COLUMN legacy_col VARCHAR")
  DBI::dbExecute(con, "CREATE TABLE alchemer.pub.legacy_table (x VARCHAR)")
  stamp_schema_version(con, schema_major, schema_minor - 1L)
  DBI::dbDisconnect(con, shutdown = TRUE)

  expect_equal(pub_layer(dir), "1")

  con2 <- alchemer_db(dir, read_only = TRUE)
  on.exit(DBI::dbDisconnect(con2, shutdown = TRUE))
  columns <- DBI::dbGetQuery(con2, "
    SELECT column_name FROM information_schema.columns
    WHERE table_catalog = 'alchemer' AND table_schema = 'pub' AND table_name = 'answers'")
  expect_false("legacy_col" %in% columns$column_name)
  expect_false(table_exists(con2, "pub", "legacy_table"))
  # Rebuilt, not merely emptied, and the new version recorded.
  expect_equal(DBI::dbGetQuery(con2, "SELECT COUNT(*) n FROM alchemer.pub.answers")$n, 4)
  expect_equal(db_schema_version(con2)$minor, schema_minor)
})

test_that("the run after a minor bump is incremental again", {
  dir <- withr::local_tempdir()
  con <- alchemer_db(dir)
  seed_survey(con)
  stamp_schema_version(con, schema_major, schema_minor - 1L)
  DBI::dbDisconnect(con, shutdown = TRUE)

  expect_equal(pub_layer(dir), "1")
  expect_equal(pub_layer(dir), character(0))
})

test_that("an unchanged run costs far fewer snapshots than a rebuild", {
  # Both halves of the change at once. Every statement outside a transaction is
  # its own DuckLake snapshot, and a snapshot costs a catalog write however few
  # rows it carries -- which is what pub_layer()'s runtime actually tracked. A
  # regression in either the skip or the per-survey transaction shows up here.
  dir <- withr::local_tempdir()
  con <- alchemer_db(dir)
  seed_survey(con, "1", "Survey One")
  seed_survey(con, "2", "Survey Two")
  DBI::dbDisconnect(con, shutdown = TRUE)

  snapshots <- function() {
    con <- alchemer_db(dir, read_only = TRUE)
    on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
    DBI::dbGetQuery(con, "SELECT COUNT(*) n FROM __ducklake_metadata_alchemer.ducklake_snapshot")$n
  }

  before <- snapshots()
  pub_layer(dir)
  full <- snapshots() - before

  before <- snapshots()
  pub_layer(dir)
  idle <- snapshots() - before

  # One transaction per survey, plus the unconditional pub.surveys rebuild.
  expect_lte(full, 4)
  expect_lte(idle, 1)
  expect_lt(idle, full)
})
