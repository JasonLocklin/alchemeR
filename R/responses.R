# Pure functions: raw response pages (parsed with simplifyVector = FALSE) plus
# the dictionary become one typed, labelled tibble.

# "1,000" -> 1000, "N/A" -> NA. Anything else that is not a plain decimal
# becomes NA with one warning naming the column and the shapes of the values.
clean_number <- function(x, name = "column") {
  x <- trimws(x)
  x[tolower(x) %in% c("", "na", "n/a", "none", "-")] <- NA
  has_commas <- grepl("^-?\\d{1,3}(,\\d{3})+(\\.\\d+)?$", x)
  x[has_commas] <- gsub(",", "", x[has_commas], fixed = TRUE)
  ok <- grepl("^-?(\\d+\\.?\\d*|\\.\\d+)$", x)
  bad <- x[!is.na(x) & !ok]
  if (length(bad) > 0) {
    # Show the shape of the values (digits -> 9, letters -> a), never the
    # values themselves: they can be identifiers.
    shape <- gsub("[A-Za-z]", "a", gsub("[0-9]", "9", bad))
    warning(
      "Column ",
      name,
      ": ",
      length(bad),
      " value(s) are not numbers and were ",
      "set to NA. Shapes: ",
      paste0("'", utils::head(unique(shape), 5), "'", collapse = ", "),
      call. = FALSE
    )
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
    # Nested grid (probe H25): subquestions{row id: {option id: element}}.
    # A checkbox row has a column per option (Q<row>_<option>); a radio row has
    # one column Q<row>. Anything else keeps the element's own id and is
    # reported as not in the definition.
    return(flat(lapply(names(el$subquestions), \(r) {
      inner <- el$subquestions[[r]]
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

parse_responses <- function(pages, dict, sso = FALSE) {
  resp <- unlist(lapply(pages, \(p) p$data), recursive = FALSE)
  cols <- unique(dict$question_id)
  one <- \(f, type) vapply(resp, f, type)

  # survey_data is [] (an empty list) on a blank response
  answers <- lapply(resp, \(r) {
    flat(lapply(r$survey_data, answer_values, cols = cols))
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
  labels <- dict[!duplicated(dict$question_id), ]
  for (col in cols) {
    x <- unname(m[, col])
    if (col %in% numeric_cols) {
      x <- clean_number(x, col)
    }
    label <- labels$label[labels$question_id == col]
    out[[col]] <- if (length(label) == 1) structure(x, label = label) else x
  }

  if (sso) {
    urls <- lapply(resp, \(r) vapply(r$url_variables, \(v) chr(v$value), ""))
    raw <- unique(unlist(lapply(urls, names)))
    safe <- paste0("url_", gsub("[^A-Za-z0-9]+", "_", raw))
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
