# Pure functions: the survey definition (parsed with simplifyVector = FALSE)
# becomes the data dictionary, which is the schema for the response columns.

# First element of x as text, NA when missing.
chr <- function(x) {
  if (length(x) == 0) NA_character_ else as.character(x[[1]])
}

# Alchemer titles can carry HTML: strip tags, decode entities, squish spaces.
clean_label <- function(x) {
  x <- gsub("<[^>]*>", "", x)
  x <- gsub("&nbsp;", " ", x, fixed = TRUE)
  x <- gsub("&lt;", "<", x, fixed = TRUE)
  x <- gsub("&gt;", ">", x, fixed = TRUE)
  x <- gsub("&quot;", "\"", x, fixed = TRUE)
  x <- gsub("&#39;", "'", x, fixed = TRUE)
  x <- gsub("&amp;", "&", x, fixed = TRUE)
  gsub("\\s+", " ", trimws(x))
}

truthy <- function(x) {
  length(x) == 1 &&
    !is.na(x) &&
    !tolower(as.character(x)) %in% c("", "0", "false")
}

# Items with one answer per option (the answers come in `options`).
multi_types <- c(
  "CHECKBOX",
  "RANK",
  "MULTI_TEXTBOX",
  "MULTI_SLIDER",
  "CONT_SUM"
)

# One row per option of every answer-holding item, in survey order. Columns:
# question_id (the data column), qid, parent, type, base_type, r_type,
# numeric_text, label, option_id, option_label, option_value (what a choice
# answer holds), page, row_id (matrix row id, NA elsewhere), order.
dictionary_from_definition <- function(def) {
  if (!"English" %in% unlist(def$languages)) {
    stop(
      "The survey has no English text; only English surveys are supported.",
      call. = FALSE
    )
  }
  title <- \(x) clean_label(chr(x$title$English))

  rows <- function(q, page, parent = NA, parent_label = NULL) {
    # MATRIX (custom table, probe 2026-11-04): rows in properties$row_names,
    # columns in sub_questions. One cell per row and column, named
    # Q<column id>_r<row number> (a checkbox column adds _<option id>).
    if (q$type == "MATRIX") {
      row_titles <- vapply(q$properties$row_names, title, "")
      row_ids <- vapply(q$properties$row_names, \(r) chr(r$id), "")
      return(unlist(
        lapply(seq_along(row_titles), \(i) {
          lapply(q$sub_questions, \(col) {
            out <- rows(
              col,
              page,
              parent = q$id,
              parent_label = paste(title(q), row_titles[i], sep = ": ")
            )
            lapply(out, \(d) {
              d$question_id <- sub(
                "^Q(\\d+)",
                paste0("Q\\1_r", i),
                d$question_id
              )
              d$row_id <- row_ids[i] # what the response cells carry
              d
            })
          }) |>
            unlist(recursive = FALSE)
        }),
        recursive = FALSE
      ))
    }
    if (length(q$sub_questions) > 0) {
      return(unlist(
        lapply(
          q$sub_questions,
          rows,
          page = page,
          parent = q$id,
          parent_label = title(q)
        ),
        recursive = FALSE
      ))
    }
    holds_answer <- (q$base_type == "Question" &&
      !q$type %in% c("INSTRUCTIONS", "MEDIA")) ||
      q$type %in% c("HIDDEN", "QUIZSCORE")
    if (!holds_answer) {
      return(list())
    }
    label <- paste(c(parent_label, title(q)), collapse = ": ")
    opts <- q$options
    if (length(opts) == 0) {
      opts <- list(list())
    }
    multi <- q$type %in% multi_types && length(q$options) > 0
    numeric_item <- q$type %in%
      c("SLIDER", "RANK", "MULTI_SLIDER", "CONT_SUM", "QUIZSCORE")
    # Text boxes set to "numbers" are not validated by Alchemer and often hold
    # ids with leading zeros: kept as text, cleaned and checked (clean_number).
    numeric_text <- q$type %in%
      c("TEXTBOX", "MULTI_TEXTBOX") &&
      truthy(q$properties$force_numeric)
    list(data.frame(
      question_id = vapply(
        opts,
        \(o) paste0("Q", q$id, if (multi) paste0("_", o$id)),
        ""
      ),
      qid = as.character(q$id),
      parent = chr(parent),
      type = q$type,
      base_type = q$base_type,
      r_type = if (numeric_item) "double" else "character",
      numeric_text = numeric_text,
      label = vapply(
        opts,
        \(o) if (multi) paste0(label, ": ", title(o)) else label,
        ""
      ),
      option_id = vapply(opts, \(o) chr(o$id), ""),
      option_label = vapply(opts, title, ""),
      option_value = vapply(opts, \(o) chr(o$value), ""),
      page = chr(page)
    ))
  }

  questions <- unlist(
    lapply(def$pages, \(p) lapply(p$questions, \(q) list(q = q, page = p$id))),
    recursive = FALSE
  )
  # The empty template keeps every column when nothing holds an answer.
  template <- dplyr::tibble(
    question_id = character(),
    qid = character(),
    parent = character(),
    type = character(),
    base_type = character(),
    r_type = character(),
    numeric_text = logical(),
    label = character(),
    option_id = character(),
    option_label = character(),
    option_value = character(),
    page = character(),
    row_id = character()
  )
  out <- dplyr::bind_rows(
    template,
    unlist(lapply(questions, \(x) rows(x$q, x$page)), recursive = FALSE)
  )
  out$order <- seq_len(nrow(out))
  dplyr::as_tibble(out)
}

# The whole survey definition: one GET (the object may or may not be
# wrapped in `data`).
get_definition <- function(survey_id, token, secret_key) {
  def <- api_get(
    paste0("survey/", survey_id),
    token = token,
    secret_key = secret_key
  )
  if (is.null(def$data)) def else def$data
}
