# Pure functions: raw response pages (parsed with simplifyVector = FALSE) plus
# the dictionary become one typed, labelled tibble.

# Cleans the text of a numeric item: "1,000" -> "1000", "N/A" -> NA, and
# student numbers "123-456-789" -> "123456789". What is still not a plain
# decimal raises one warning naming the column and the shapes of the values.
# as_text = TRUE (text boxes set to numbers) returns the cleaned text, leading
# zeros kept and odd values left as they are; otherwise a number, odd values NA.
# A regular expression decides what is a number, not as.numeric(), which
# would also accept "1e5", "0x10" and "Inf".
clean_number <- function(x, name = "column", as_text = FALSE) {
  x <- trimws(x)
  x[tolower(x) %in% c("", "na", "n/a", "none", "-")] <- NA
  # Student numbers are often written 123-456-789: drop those dashes only
  x <- sub("^\\(?(\\d{3})-(\\d{3})-(\\d{3})\\)?$", "\\1\\2\\3", x)
  has_commas <- grepl("^-?\\d{1,3}(,\\d{3})+(\\.\\d+)?$", x)
  x[has_commas] <- gsub(",", "", x[has_commas], fixed = TRUE)
  ok <- grepl("^-?(\\d+\\.?\\d*|\\.\\d+)$", x)
  bad <- x[!is.na(x) & !ok]
  if (length(bad) > 0) {
    # The shape of the values (digits -> 9, letters -> a), never the values
    # themselves: they can be identifiers.
    shapes <- unique(gsub("[A-Za-z]", "a", gsub("[0-9]", "9", bad)))
    warning(
      paste0(
        "Column ",
        name,
        ": ",
        length(bad),
        " value(s) are not numbers",
        if (as_text) " (kept as text)." else " and were set to NA.",
        " Shapes: ",
        paste0("'", utils::head(shapes, 5), "'", collapse = ", ")
      ),
      call. = FALSE
    )
  }
  if (as_text) {
    return(x)
  }
  out <- rep(NA_real_, length(x))
  out[ok] <- as.numeric(x[ok])
  out
}

# API timestamps are GMT (= UTC): read them as UTC, show them in Toronto.
parse_time <- function(x) {
  x[x %in% c("", "0000-00-00 00:00:00")] <- NA
  suffix <- trimws(sub("^\\d{4}-\\d{2}-\\d{2} \\d{2}:\\d{2}:\\d{2}", "", x))
  if (any(!is.na(suffix) & !suffix %in% c("", "GMT"))) {
    stop(
      "Unexpected time zone in API dates: ",
      paste(unique(suffix[!suffix %in% c("", "GMT")]), collapse = ", "),
      call. = FALSE
    )
  }
  lubridate::ymd_hms(sub("\\s*GMT$", "", x), tz = "UTC", quiet = TRUE) |>
    lubridate::with_tz("America/Toronto")
}

to_json <- function(x) as.character(jsonlite::toJSON(x, auto_unbox = TRUE))

# unlist() that keeps only the innermost names (column names).
flat <- \(x) unlist(unname(x))

# Named character vector (column -> value) for one survey_data element.
answer_values <- function(el, cols, row = NULL) {
  a <- el$answer
  # Real data: checkbox and grid elements have type "parent" and carry their
  # answers in `options` / `subquestions`, so the type is not checked here.
  if (length(el$subquestions) > 0) {
    # Grid rows come in two shapes. A row can be one element
    # (subquestions{row id: element}), mapped by its own id like a top-level
    # element. Or a map of option cells
    # (subquestions{row id: {option id: element}}): a checkbox row has a column
    # per option (Q<row>_<option>), a radio row one column Q<row>. Anything
    # else keeps the element's own id and is reported as not in the definition.
    return(flat(lapply(names(el$subquestions), \(r) {
      inner <- el$subquestions[[r]]
      if (is.list(inner) && "id" %in% names(inner)) {
        return(answer_values(inner, cols))
      }
      flat(lapply(names(inner), \(k) {
        e <- inner[[k]]
        if (paste0("Q", r, "_", k) %in% cols) {
          e$id <- paste0(r, "_", k)
        } else if (paste0("Q", r) %in% cols) {
          e$id <- r
        }
        answer_values(e, cols)
      }))
    })))
  }
  if (length(el$options) > 0) {
    return(flat(lapply(el$options, \(o) {
      stats::setNames(chr(o$answer), paste0("Q", el$id, "_", o$id))
    })))
  }
  value <- if (!is.list(a) || length(a) == 0) {
    chr(a)
  } else if (!is.null(a$signature)) {
    chr(a$name)
  } else if (is.list(a[[1]]) && !is.null(a[[1]]$rank)) {
    return(flat(lapply(a, \(r) {
      stats::setNames(chr(r$rank), paste0("Q", el$id, "_", r$id))
    })))
  } else {
    to_json(a)
  }
  stats::setNames(value, paste0("Q", el$id))
}

# MATRIX cells come keyed 10001, 10002, ... (ids not in the definition).
# Each cell's piped_value is the id of its row (checked on 8 154
# real cells), and a response may hold only some cells (rows shown by
# piping). A cell's column is the first unused column of its row whose type
# fits (the cells of a row come in column order); a cell that has no row, no
# fitting column, or could fit several columns while some are missing, leaves
# the element as it is: its answers are then kept under their own ids with
# the "not in the survey definition" warning, never placed wrongly.
matrix_cells <- function(el, m) {
  cells <- el$subquestions[order(as.numeric(names(el$subquestions)))]
  row <- match(vapply(cells, \(x) chr(x$piped_value), ""), m$row_ids)
  col <- rep(NA_integer_, length(cells))
  for (r in unique(row[!is.na(row)])) {
    in_row <- which(row == r)
    for (i in in_row) {
      type <- chr(cells[[i]]$type)
      fits <- which(
        (m$types == type | (type == "parent" & m$types %in% multi_types)) &
          !seq_along(m$cols) %in% col[in_row]
      )
      if (
        length(fits) == 1 ||
          (length(fits) > 1 && length(in_row) == length(m$cols))
      ) {
        col[i] <- fits[1]
      }
    }
  }
  if (anyNA(row) || anyNA(col)) {
    return(el)
  }
  ids <- paste0(m$cols[col], "_r", row)
  el$subquestions <- stats::setNames(
    Map(\(x, id) utils::modifyList(x, list(id = id)), cells, ids),
    ids
  )
  el
}

# The MATRIX items of a dictionary: columns in order, their types, row ids.
matrices_of <- function(dict) {
  d <- dict[!is.na(dict$row_id), ]
  lapply(split(d, d$parent), \(x) {
    cols <- unique(x$qid)
    list(
      cols = cols,
      types = x$type[match(cols, x$qid)],
      row_ids = unique(x$row_id)
    )
  })
}

# Number of non-empty answers anywhere in a survey_data object: every `answer`
# field that holds something, a RANK answer counted once per ranked option. It
# does not depend on element types, so it still counts what the parser cannot place.
count_answers <- function(x) {
  if (!is.list(x)) {
    return(0)
  }
  sum(vapply(
    seq_along(x),
    \(i) {
      v <- x[[i]]
      if (!identical(names(x)[i], "answer")) {
        count_answers(v)
      } else if (length(v) == 0) {
        0
      } else if (!is.list(v)) {
        as.numeric(!is.na(v[[1]]) && as.character(v[[1]]) != "")
      } else if (!is.null(v$signature)) {
        as.numeric(!is.na(chr(v$name)) && nzchar(chr(v$name)))
      } else if (is.list(v[[1]]) && !is.null(v[[1]]$rank)) {
        sum(vapply(v, \(r) !is.na(chr(r$rank)) && nzchar(chr(r$rank)), NA))
      } else {
        1
      }
    },
    0
  ))
}

parse_responses <- function(pages, dict, sso = FALSE) {
  resp <- unlist(lapply(pages, \(p) p$data), recursive = FALSE)
  cols <- unique(dict$question_id)
  one <- \(f, type) vapply(resp, f, type)

  # survey_data is [] (an empty list) on a blank response
  matrices <- matrices_of(dict)
  answers <- lapply(resp, \(r) {
    flat(lapply(r$survey_data, \(el) {
      m <- matrices[[chr(el$id)]]
      if (!is.null(m)) {
        el <- matrix_cells(el, m)
      }
      answer_values(el, cols)
    }))
  })
  answers <- lapply(answers, \(a) a[!is.na(a)]) # a later NA must not overwrite
  n <- vapply(answers, length, 1L)
  found <- unlist(lapply(answers, names))
  extra <- setdiff(unique(found), cols)
  cols <- c(cols, extra)
  if (length(extra) > 0) {
    warning(
      "Answers not in the survey definition were kept: ",
      paste(extra, collapse = ", "),
      call. = FALSE
    )
  }
  m <- matrix(
    NA_character_,
    length(resp),
    length(cols),
    dimnames = list(NULL, cols)
  )
  m[cbind(rep(seq_along(resp), n), match(found, cols))] <- unlist(answers)
  m[!is.na(m) & m == ""] <- NA

  # Every non-empty answer in the raw data must have landed in a column
  raw_answers <- sum(vapply(resp, \(r) count_answers(r$survey_data), 0))
  if (raw_answers > sum(!is.na(m))) {
    warning(
      raw_answers - sum(!is.na(m)),
      " of ",
      raw_answers,
      " answers in the raw data were not placed in a column. ",
      "The survey has an answer shape this package does not know; please report it.",
      call. = FALSE
    )
  }

  out <- dplyr::tibble(
    id = one(\(r) as.integer(r$id), 1L),
    status = one(\(r) chr(r$status), ""),
    is_test_data = one(\(r) identical(chr(r$is_test_data), "1"), TRUE),
    date_started = parse_time(one(\(r) chr(r$date_started), "")),
    date_submitted = parse_time(one(\(r) chr(r$date_submitted), "")),
    date_updated = parse_time(one(\(r) chr(r$date_updated), "")),
    response_time = one(\(r) as.numeric(chr(r$response_time)), 1),
    language = one(\(r) chr(r$language), ""),
    link_id = one(\(r) chr(r$link_id), ""),
    data_quality = one(
      \(r) {
        if (length(r$data_quality) == 0) {
          NA_character_
        } else {
          to_json(r$data_quality)
        }
      },
      ""
    )
  )

  numeric_cols <- unique(dict$question_id[dict$r_type == "double"])
  numeric_text <- unique(dict$question_id[dict$numeric_text %in% TRUE])
  labels <- dict[!duplicated(dict$question_id), ]
  for (col in cols) {
    x <- unname(m[, col])
    if (col %in% numeric_cols) {
      x <- clean_number(x, col)
    } else if (col %in% numeric_text) {
      x <- clean_number(x, col, as_text = TRUE)
    }
    label <- labels$label[labels$question_id == col]
    out[[col]] <- if (length(label) == 1) structure(x, label = label) else x
  }

  if (sso) {
    urls <- lapply(resp, \(r) vapply(r$url_variables, \(v) chr(v$value), ""))
    raw <- unique(unlist(lapply(urls, names)))
    # URL-shaped claim names (http://.../claims/givenname) use their last segment
    # when that is unique; any clash keeps the full name.
    full <- gsub("[^A-Za-z0-9]+", "_", raw)
    short <- ifelse(
      grepl("/", raw),
      gsub("[^A-Za-z0-9]+", "_", sub(".*/", "", sub("/+$", "", trimws(raw)))),
      full
    )
    safe <- paste0(
      "url_",
      ifelse(short %in% short[duplicated(short)], full, short)
    )
    if (anyDuplicated(safe)) {
      stop(
        "URL variable names collide after cleaning: ",
        paste(raw, collapse = ", "),
        call. = FALSE
      )
    }
    for (i in seq_along(raw)) {
      out[[safe[i]]] <- vapply(
        urls,
        \(u) if (raw[i] %in% names(u)) u[[raw[i]]] else NA_character_,
        ""
      )
    }
  }
  out
}
