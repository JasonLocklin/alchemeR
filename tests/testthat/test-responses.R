dict <- function() dictionary_from_definition(definition())

test_that("every answer shape lands in its column, in dictionary order", {
  res <- parse_responses(pages_of(response(1, full_answers())), dict())
  out <- lapply(res, as.vector) # drop the label attributes
  expect_equal(nrow(res), 1)
  expect_equal(
    names(out)[-(1:10)],
    c(
      "Q3",
      "Q4_10005",
      "Q4_10006",
      "Q6",
      "Q7",
      "Q9",
      "Q12_10020",
      "Q13",
      "Q14",
      "Q15_10030",
      "Q15_10031",
      "Q16"
    )
  )
  expect_equal(out$Q3, "1") # reporting value, not the title
  expect_equal(out$Q4_10005, "Apple")
  expect_true(is.na(out$Q4_10006))
  expect_equal(out$Q6, 34)
  expect_equal(out$Q9, "Yes")
  expect_equal(out$Q12_10020, "Opt X")
  expect_equal(out$Q13, "Pat") # signature: name only
  expect_equal(out$Q14, "abc")
  expect_equal(c(out$Q15_10030, out$Q15_10031), c(2, 1))
  expect_equal(out$Q16, 7)
  expect_false(any(grepl("base64", unlist(out))))
})

test_that("system columns are typed and dates are Toronto instants", {
  out <- parse_responses(pages_of(response(1, full_answers())), dict())
  expect_type(out$id, "integer")
  expect_false(out$is_test_data)
  expect_s3_class(out$date_submitted, "POSIXct")
  expect_equal(attr(out$date_submitted, "tzone"), "America/Toronto")
  expect_equal(format(out$date_submitted, tz = "UTC"), "2026-11-01 05:31:00")
  expect_equal(format(out$date_updated, tz = "UTC"), "2026-11-01 05:31:05")
  expect_true(is.na(out$data_quality))
})

test_that("labels are attached to answer columns", {
  out <- parse_responses(pages_of(response(1, full_answers())), dict())
  expect_equal(attr(out$Q3, "label"), "Q3 colour ?")
  expect_equal(attr(out$Q4_10005, "label"), "Fruit: Apple")
  expect_equal(attr(out$Q9, "label"), "Grid: Row 1")
})

test_that("a blank response (survey_data []) keeps every answer column", {
  out <- parse_responses(
    pages_of(
      response(1, full_answers()),
      response(2, list(), status = "Partial")
    ),
    dict()
  )
  expect_equal(nrow(out), 2)
  expect_equal(sum(grepl("^Q", names(out))), 12)
  expect_true(all(is.na(out[2, grepl("^Q", names(out))])))
})

test_that("zero rows keep the column set and types", {
  out <- parse_responses(list(list(data = list())), dict())
  expect_equal(nrow(out), 0)
  expect_equal(sum(grepl("^Q", names(out))), 12)
  expect_type(out$Q6, "double")
  expect_s3_class(out$date_updated, "POSIXct")
})

test_that("key drift, partial and ids out of order are handled", {
  a <- full_answers()
  a$`14` <- NULL
  out <- parse_responses(
    pages_of(
      response(9, a),
      response(2, full_answers(), status = "Disqualified")
    ),
    dict()
  )
  expect_equal(out$id, c(9L, 2L))
  expect_equal(out$status, c("Complete", "Disqualified"))
  expect_equal(as.vector(out$Q14), c(NA, "abc"))
})

test_that("numbers are cleaned; what is not a number becomes NA with one warning", {
  rows <- list(
    response(1, full_answers("1,000")),
    response(2, full_answers("N/A")),
    response(3, full_answers(" 12 ")),
    response(4, full_answers("-")),
    response(5, full_answers(".5"))
  )
  out <- parse_responses(list(list(data = rows)), dict())
  expect_equal(as.vector(out$Q6), c(1000, NA, 12, NA, 0.5))

  rows <- list(
    response(1, full_answers("12 kg")),
    response(2, full_answers("0x10")),
    response(3, full_answers("1,5"))
  )
  expect_warning(
    out <- parse_responses(list(list(data = rows)), dict()),
    "Column Q6: 3 value.*'99 aa', '9a99', '9,9'"
  )
  expect_true(all(is.na(out$Q6)))
})

test_that("the warning never shows the raw values", {
  rows <- list(response(1, full_answers("940-162-720")))
  w <- expect_warning(parse_responses(list(list(data = rows)), dict()))
  expect_false(grepl("940", conditionMessage(w)))
  expect_match(conditionMessage(w), "999-999-999")
})

test_that("answers missing from the definition are kept with a warning", {
  a <- full_answers()
  a$`99` <- list(id = 99, type = "TEXTBOX", answer = "new")
  expect_warning(
    out <- parse_responses(pages_of(response(1, a)), dict()),
    "Q99"
  )
  expect_equal(out$Q99, "new")
  expect_equal(tail(names(out), 1), "Q99")
})

test_that("an unexpected time zone suffix is an error", {
  r <- response(1, full_answers(), date_submitted = "2026-11-01 05:31:00 EDT")
  expect_error(parse_responses(pages_of(r), dict()), "time zone")
})

test_that("the DST fall-back hour stays two instants after a file round trip", {
  rows <- list(
    response(1, full_answers(), date_submitted = "2026-11-01 05:30:00 GMT"),
    response(2, full_answers(), date_submitted = "2026-11-01 06:30:00 GMT")
  )
  out <- parse_responses(list(list(data = rows)), dict())
  f <- withr::local_tempfile(fileext = ".rds")
  saveRDS(out, f)
  back <- readRDS(f)
  expect_equal(
    format(back$date_submitted, "%H:%M %Z"),
    c("01:30 EDT", "01:30 EST")
  )
  expect_equal(length(unique(back$date_submitted)), 2)
  expect_equal(attr(back$Q3, "label"), "Q3 colour ?")
})

test_that("url variables appear for SSO surveys only, with safe names", {
  uv <- list(
    `https://idp/claim/email` = list(
      key = "k",
      value = "a@b.org",
      type = "url"
    ),
    src = list(key = "src", value = "x", type = "url")
  )
  rows <- list(
    response(1, full_answers(), url_variables = uv),
    response(2, full_answers())
  )
  out <- parse_responses(list(list(data = rows)), dict(), sso = TRUE)
  expect_equal(out$url_https_idp_claim_email, c("a@b.org", NA))
  expect_equal(out$url_src, c("x", NA))
  expect_false(any(grepl(
    "^url_",
    names(parse_responses(list(list(data = rows)), dict()))
  )))

  clash <- list(a.b = list(value = "1"), `a-b` = list(value = "2"))
  expect_error(
    parse_responses(
      pages_of(response(1, full_answers(), url_variables = clash)),
      dict(),
      sso = TRUE
    ),
    "collide"
  )
})

test_that("data_quality becomes JSON text", {
  r <- response(
    1,
    full_answers(),
    data_quality = list(checkbox = list(all = list(4)))
  )
  out <- parse_responses(pages_of(r), dict())
  expect_match(out$data_quality, "checkbox")
})

test_that("a 10 000-row page parses in reasonable time", {
  rows <- lapply(seq_len(10000), \(i) response(i, full_answers()))
  t <- system.time(out <- parse_responses(list(list(data = rows)), dict()))[[
    "elapsed"
  ]]
  expect_equal(nrow(out), 10000)
  expect_lt(t, 20)
})

test_that("a radio grid row maps to one column, whichever option cell holds it", {
  a <- full_answers()
  a$`11` <- NULL
  a$`8` <- list(
    id = 8,
    type = "TABLE",
    subquestions = list(
      `9` = list(
        `10010` = list(id = 10010, parent = 9, answer = NA),
        `10011` = list(id = 10011, parent = 9, answer = "Yes")
      )
    )
  )
  a$`9` <- NULL
  out <- parse_responses(pages_of(response(1, a)), dict())
  expect_equal(as.vector(out$Q9), "Yes")
})
