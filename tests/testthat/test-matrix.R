# MATRIX (custom table) and QUIZSCORE in the shapes of the real API:
# rows in properties$row_names, columns in
# sub_questions, response cells keyed by ids that are not in the definition,
# column by column.
matrix_def <- function() {
  def <- definition()
  row <- \(id, title) list(id = id, title = list(English = title), value = "x")
  def$pages[[1]]$questions <- c(
    def$pages[[1]]$questions,
    list(
      item(
        30,
        "MATRIX",
        "Matrix",
        properties = list(
          row_names = list(row("a1b2", "Row A"), row("c3d4", "Row B"))
        ),
        sub_questions = list(
          item(31, "RADIO", "Like?", list(opt(10035, "Agree", "agree 1"))),
          item(32, "ESSAY", "Why"),
          item(33, "CHECKBOX", "Tick", list(opt(10100, "Done")))
        )
      ),
      item(40, "QUIZSCORE", "Score", base_type = "Action")
    )
  )
  def
}

cell <- \(id, type, row, ...) {
  list(id = id, type = type, question = "q", piped_value = row, ...)
}
matrix_answers <- function() {
  a <- full_answers()
  a$`30` <- list(
    id = 30,
    type = "parent",
    question = "Matrix",
    subquestions = list(
      `10002` = cell(
        10002,
        "RADIO",
        "a1b2",
        answer = "agree 1",
        answer_id = 10035
      ),
      `10003` = cell(10003, "RADIO", "c3d4", answer = NULL),
      `10004` = cell(10004, "ESSAY", "a1b2", answer = "because"),
      `10005` = cell(10005, "ESSAY", "c3d4", answer = "so"),
      `10006` = cell(
        10006,
        "parent",
        "a1b2",
        options = list(
          `10100` = list(id = 10100, option = "Done", answer = "Done")
        )
      ),
      `10007` = cell(10007, "parent", "c3d4", options = list())
    )
  )
  a$`40` <- list(id = 40, type = "QUIZSCORE", answer = "12", answer_id = 0)
  a
}

test_that("a matrix has one column per row and column, labelled", {
  d <- dictionary_from_definition(matrix_def())
  m <- d[d$parent %in% "30", ]
  expect_equal(
    unique(m$question_id),
    c("Q31_r1", "Q32_r1", "Q33_r1_10100", "Q31_r2", "Q32_r2", "Q33_r2_10100")
  )
  expect_equal(m$label[m$question_id == "Q32_r2"], "Matrix: Row B: Why")
  expect_false(any(d$question_id %in% c("Q31", "Q32", "Q33_10100")))
})

test_that("matrix cells land in their row and column", {
  expect_no_warning(
    out <- parse_responses(
      pages_of(response(1, matrix_answers())),
      dictionary_from_definition(matrix_def())
    )
  )
  expect_equal(as.vector(out$Q31_r1), "agree 1") # reporting value
  expect_true(is.na(out$Q31_r2))
  expect_equal(c(out$Q32_r1, out$Q32_r2), c("because", "so"))
  expect_equal(as.vector(out$Q33_r1_10100), "Done")
  expect_true(is.na(out$Q33_r2_10100))
  expect_equal(attr(out$Q32_r1, "label"), "Matrix: Row A: Why")
})

test_that("a matrix with only some cells (piped rows) still lands right", {
  a <- matrix_answers()
  a$`30`$subquestions <- a$`30`$subquestions[c("10003", "10004")]
  expect_no_warning(
    out <- parse_responses(
      pages_of(response(1, a)),
      dictionary_from_definition(matrix_def())
    )
  )
  expect_equal(as.vector(out$Q32_r1), "because") # row from piped_value
  expect_true(all(is.na(c(out$Q31_r1, out$Q32_r2, out$Q33_r1_10100))))
})

test_that("a cell with an unknown row or an ambiguous column is never placed", {
  a <- matrix_answers()
  a$`30`$subquestions$`10004`$piped_value <- "zzzz"
  expect_warning(
    out <- parse_responses(
      pages_of(response(1, a)),
      dictionary_from_definition(matrix_def())
    ),
    "not in the survey definition"
  )
  expect_equal(as.vector(out$Q10004), "because")

  def <- matrix_def() # two text columns; row 1 holds one text cell
  i <- which(vapply(def$pages[[1]]$questions, \(q) q$id == 30, NA))
  def$pages[[1]]$questions[[i]]$sub_questions[[3]] <- item(33, "ESSAY", "How")
  a <- matrix_answers()
  a$`30`$subquestions <- a$`30`$subquestions["10004"]
  expect_warning(
    parse_responses(pages_of(response(1, a)), dictionary_from_definition(def)),
    "not in the survey definition"
  )
})

test_that("a quiz score is a number", {
  d <- dictionary_from_definition(matrix_def())
  expect_equal(unique(d$r_type[d$question_id == "Q40"]), "double")
  out <- parse_responses(pages_of(response(1, matrix_answers())), d)
  expect_equal(as.vector(out$Q40), 12)
})
