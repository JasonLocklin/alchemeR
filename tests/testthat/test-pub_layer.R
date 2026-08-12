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

test_that("date_submitted/date_started convert their EST/EDT suffix to identical Toronto wall-clock time", {
  # EST/EDT *are* Toronto's own standard/daylight offsets, so a properly
  # parsed EST/EDT-suffixed timestamp should land at the same wall-clock
  # value in America/Toronto -- winter (EST) and summer (EDT) both checked,
  # since a naive fixed-offset assumption would get one of the two wrong.
  dir <- withr::local_tempdir()
  con <- alchemer_db(dir)
  DBI::dbExecute(con, "INSERT INTO alchemer.raw.surveys (survey_id, title, is_deleted) VALUES ('1', 'S', FALSE)")
  DBI::dbExecute(con, "INSERT INTO alchemer.raw.responses
    (survey_id, response_id, date_submitted, date_started, is_deleted) VALUES
    ('1', 'winter', '2026-01-15 14:30:00 EST', '2026-01-15 14:25:00 EST', FALSE),
    ('1', 'summer', '2026-07-15 14:30:00 EDT', '2026-07-15 14:25:00 EDT', FALSE)")
  DBI::dbDisconnect(con, shutdown = TRUE)

  pub_layer(dir)

  con2 <- alchemer_db(dir, read_only = TRUE)
  on.exit(DBI::dbDisconnect(con2, shutdown = TRUE))
  responses <- DBI::dbGetQuery(con2, "
    SELECT response_id, date_submitted, date_started FROM alchemer.pub.responses ORDER BY response_id")
  expect_equal(format(responses$date_submitted[responses$response_id == "winter"]), "2026-01-15 14:30:00")
  expect_equal(format(responses$date_submitted[responses$response_id == "summer"]), "2026-07-15 14:30:00")
  expect_equal(format(responses$date_started[responses$response_id == "winter"]), "2026-01-15 14:25:00")
})

test_that("date_updated/created_on/modified_on convert their assumed-UTC value to Toronto wall-clock time", {
  # No suffix at all on these three, so they're assumed UTC and shifted --
  # -5h in winter (EST), -4h in summer (EDT) -- rather than left naive.
  dir <- withr::local_tempdir()
  con <- alchemer_db(dir)
  DBI::dbExecute(con, "INSERT INTO alchemer.raw.surveys
    (survey_id, title, created_on, modified_on, is_deleted) VALUES
    ('1', 'S', '2026-01-15 19:00:00', '2026-07-15 18:00:00', FALSE)")
  DBI::dbExecute(con, "INSERT INTO alchemer.raw.responses
    (survey_id, response_id, date_updated, is_deleted) VALUES
    ('1', 'r1', '2026-01-15 19:00:00', FALSE)")
  DBI::dbDisconnect(con, shutdown = TRUE)

  pub_layer(dir)

  con2 <- alchemer_db(dir, read_only = TRUE)
  on.exit(DBI::dbDisconnect(con2, shutdown = TRUE))
  surveys <- DBI::dbGetQuery(con2, "SELECT created_on, modified_on FROM alchemer.pub.surveys")
  expect_equal(format(surveys$created_on), "2026-01-15 14:00:00") # UTC-5 (EST)
  expect_equal(format(surveys$modified_on), "2026-07-15 14:00:00") # UTC-4 (EDT)

  responses <- DBI::dbGetQuery(con2, "SELECT date_updated FROM alchemer.pub.responses")
  expect_equal(format(responses$date_updated), "2026-01-15 14:00:00")
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

test_that("pub_layer(language =) rejects a value that could break the generated SQL", {
  # Regression test (ultrareview): title_sql() interpolated `language`
  # directly into a SQL/JSONPath string with no escaping.
  dir <- withr::local_tempdir()
  con <- alchemer_db(dir)
  seed_survey(con)
  DBI::dbDisconnect(con, shutdown = TRUE)

  expect_error(pub_layer(dir, language = "English\") --"), class = "alchemeR_config_error")
  expect_error(pub_layer(dir, language = "en'glish"), class = "alchemeR_config_error")
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
