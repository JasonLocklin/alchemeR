test_that("the dictionary lists answer-holding items in survey order", {
  d <- dictionary_from_definition(definition())
  expect_equal(
    unique(d$question_id),
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
  expect_equal(d$order, seq_len(nrow(d)))
  expect_false(anyNA(d$page))
  # Action EMAIL, INSTRUCTIONS, MEDIA and the table parents hold no answers
  expect_false(any(c("Q5", "Q8", "Q11", "Q17", "Q18") %in% d$question_id))
})

test_that("labels are clean and option values are kept next to titles", {
  d <- dictionary_from_definition(definition())
  q3 <- d[d$question_id == "Q3", ]
  expect_equal(unique(q3$label), "Q3 colour ?")
  expect_equal(q3$option_value, c("1", "2"))
  expect_equal(q3$option_label, c("Red", "Blue"))
  expect_equal(d$label[d$question_id == "Q4_10006"], "Fruit: Other")
  expect_equal(d$label[d$question_id == "Q9"], "Grid: Row 1")
  expect_equal(d$parent[d$question_id == "Q9"], "8")
  expect_equal(d$label[d$question_id == "Q12_10020"], "Grid2: Row A: Opt X")
})

test_that("clean_label strips tags and entities", {
  expect_equal(
    clean_label(
      "<span style='x'>A &amp; B</span>&nbsp; &lt;1 &quot;q&quot; it&#39;s\n\t ok"
    ),
    "A & B <1 \"q\" it's ok"
  )
})

test_that("r_type follows the item type, never the content", {
  d <- dictionary_from_definition(definition())
  type <- \(id) unique(d$r_type[d$question_id == id])
  expect_equal(type("Q6"), "character") # force_numeric textbox: text, checked
  expect_true(d$numeric_text[d$question_id == "Q6"])
  expect_false(d$numeric_text[d$question_id == "Q7"])
  expect_equal(type("Q7"), "character") # plain textbox
  expect_equal(type("Q3"), "character") # numeric-looking choice codes stay text
  expect_equal(type("Q15_10030"), "double") # rank
  expect_equal(type("Q16"), "double") # slider
})

test_that("mixed option id/value classes do not break the dictionary", {
  def <- definition()
  def$pages[[1]]$questions[[1]]$options <- list(
    list(id = 1L, title = list(English = "a"), value = 1L),
    list(id = "2", title = list(English = "b"), value = "x")
  )
  d <- dictionary_from_definition(def)
  expect_equal(d$option_id[d$question_id == "Q3"], c("1", "2"))
  expect_equal(d$option_value[d$question_id == "Q3"], c("1", "x"))
})

test_that("a survey without English text is an error", {
  def <- definition()
  def$languages <- list("French")
  expect_error(dictionary_from_definition(def), "English")
})

test_that("fetch_data_dictionary makes one GET of the definition", {
  calls <- mock_api(\(req) json(list(result_ok = TRUE, data = definition())))
  d <- fetch_data_dictionary(123)
  expect_length(calls$reqs, 1)
  expect_match(calls$reqs[[1]]$url, "/survey/123\\?")
  expect_true("Q3" %in% d$question_id)
  # unwrapped single object
  mock_api(\(req) json(c(list(result_ok = TRUE), definition())))
  expect_true("Q3" %in% fetch_data_dictionary(123)$question_id)
})

test_that("no answer-holding items: empty dictionary, all columns", {
  def <- definition()
  def$pages[[1]]$questions <- list(item(17, "INSTRUCTIONS", "Read this"))
  expect_no_warning(d <- dictionary_from_definition(def))
  expect_equal(nrow(d), 0)
  expect_true(all(c("question_id", "r_type", "label", "order") %in% names(d)))
  expect_no_warning(out <- parse_responses(pages_of(), d))
  expect_equal(nrow(out), 0)
})
