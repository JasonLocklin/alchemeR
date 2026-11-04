#' Fetch Alchemer Survey Data
#'
#' Downloads the responses of one survey into a labelled tibble and keeps a
#' copy in `survey_<id>.rds` in `dir`, so the data can be read again without
#' calling the API. Requires an Alchemer API token and secret key.
#'
#' @param survey_id
#' Alchemer survey number, retrieved from the survey URL.
#'
#' @param token
#' Alchemer API token (https://apihelp.alchemer.com/help/authentication).
#' Defaults to the environment variable `ALCHEMER_API_TOKEN`.
#'
#' @param secret_key
#' Alchemer API secret key. Defaults to the environment variable
#' `ALCHEMER_API_SECRET`.
#'
#' @param dir
#' Folder for the `survey_<id>.rds` file.
#'
#' @param refresh
#' `"auto"` (default) uses the local file if it was written less than
#' `min_age` seconds ago and pulls from Alchemer otherwise; `"full"` always
#' pulls everything; `"local"` reads the file and makes no API call.
#'
#' @param min_age
#' Seconds during which the local file counts as fresh.
#'
#' @param full_refresh_days
#' Pull everything again if the oldest rows are older than this many days and
#' the survey received new or changed responses.
#'
#' @param include_test
#' Include responses marked as test data?
#'
#' @return
#' A tibble with one row per response: `survey_id`, system columns, one column
#' per question (named `Q<id>`, or `Q<id>_<option id>` for checkbox and rank
#' items; the question text is the column's `label` attribute), `url_*` columns
#' for single sign-on surveys, and `fetched_at`. `attr(x, "alchemer")` holds the
#' survey metadata and the data dictionary.
#' @export
#'
#' @examples
#' \dontrun{
#' df <- fetch_survey(123)
#' }
fetch_survey <- function(
  survey_id,
  token = Sys.getenv("ALCHEMER_API_TOKEN"),
  secret_key = Sys.getenv("ALCHEMER_API_SECRET"),
  dir = ".",
  refresh = c("auto", "full", "local"),
  min_age = 600,
  full_refresh_days = 90,
  include_test = FALSE
) {
  refresh <- match.arg(refresh)
  survey_id <- format(survey_id, scientific = FALSE, trim = TRUE)
  if (!grepl("^[0-9]+$", survey_id)) {
    stop("survey_id must be the survey number.", call. = FALSE)
  }
  path <- file.path(dir, paste0("survey_", survey_id, ".rds"))
  started <- Sys.time()
  requests <- api_stats$requests

  say <- function(...) {
    message(
      "Survey ",
      survey_id,
      ": ",
      ...,
      ", ",
      api_stats$requests - requests,
      " requests, ",
      format_seconds(as.numeric(Sys.time() - started, units = "secs"))
    )
  }

  if (refresh == "local") {
    if (!file.exists(path)) {
      stop("No local file ", path, "; use refresh = \"full\".", call. = FALSE)
    }
    out <- read_local(path, survey_id)
    say("local file (", nrow(out), " rows)")
    return(out)
  }

  local <- if (refresh == "full") {
    NULL
  } else {
    tryCatch(read_local(path, survey_id), error = \(e) NULL)
  }
  meta <- attr(local, "alchemer")
  reason <- full_reason(refresh, local, include_test)

  # A fresh, usable file is returned without a request.
  age <- as.numeric(Sys.time() - file.mtime(path), units = "secs")
  if (is.null(reason) && !is.na(age) && age >= 0 && age < min_age) {
    say("local file (checked ", round(age / 60), " min ago)")
    return(local)
  }

  def <- NULL
  if (is.null(reason)) {
    def <- get_definition(survey_id, token, secret_key)
    if (!identical(chr(def$modified_on), meta$modified_on)) {
      reason <- "the survey was edited"
    }
  }
  if (is.null(reason)) {
    update <- update_local(
      local,
      def,
      survey_id,
      token,
      secret_key,
      include_test,
      full_refresh_days
    )
    reason <- update$reason
  }

  if (is.null(reason)) {
    if (update$n_changed == 0 && nrow(update$data) == nrow(local)) {
      # Nothing changed: record that we checked, without rewriting the file.
      suppressWarnings(Sys.setFileTime(path, Sys.time()))
      say(
        "unchanged (latest update ",
        format(meta$max_date_updated, "%Y-%m-%d %H:%M"),
        ")"
      )
      return(local)
    }
    write_local(update$data, path)
    say("incremental pull (", update$n_changed, " rows changed)")
    return(update$data)
  }

  if (!is.null(local)) {
    message(
      "Survey ",
      survey_id,
      ": full refresh needed (",
      reason,
      "), about ",
      ceiling(nrow(local) / 500) + 3,
      " requests."
    )
  }
  out <- pull_survey(survey_id, token, secret_key, include_test, def)
  write_local(out, path)
  say("full pull (", reason, "), ", nrow(out), " rows")
  out
}

# Why the local file cannot be updated incrementally; NULL if it can.
full_reason <- function(refresh, local, include_test) {
  meta <- attr(local, "alchemer")
  if (refresh == "full") {
    "refresh requested"
  } else if (is.null(local)) {
    "no usable local file"
  } else if (!identical(meta$include_test, include_test)) {
    "include_test changed"
  } else if (!identical(meta$schema_version, 1L)) {
    "the file format changed"
  } else if (is.na(meta$max_date_updated)) {
    "no stored update time"
  }
}

# Filters for the response list: each argument is c(field, operator, value).
filter_query <- function(...) {
  filters <- list(...)
  names(filters) <- NULL
  out <- lapply(seq_along(filters), \(i) {
    stats::setNames(
      filters[[i]],
      paste0("filter[", c("field", "operator", "value"), "][", i - 1, "]")
    )
  })
  as.list(unlist(out))
}

# Filters shared by every response request of a survey.
base_filters <- function(include_test) {
  if (include_test) list() else list(c("is_test_data", "=", "0"))
}

utc_string <- function(x) format(x, "%Y-%m-%d %H:%M:%S", tz = "UTC")

# One complete pull: definition (if not fetched yet), then every response page.
pull_survey <- function(
  survey_id,
  token,
  secret_key,
  include_test,
  def = NULL
) {
  if (is.null(def)) {
    def <- get_definition(survey_id, token, secret_key)
  }
  dict <- dictionary_from_definition(def)
  pages <- api_get_all(
    paste0("survey/", survey_id, "/surveyresponse"),
    do.call(filter_query, base_filters(include_test)),
    token = token,
    secret_key = secret_key,
    simplify = FALSE
  )
  out <- parse_pull(pages, dict, def, survey_id)
  finish(out, dict, def, survey_id, include_test, pages[[1]]$total_count)
}

# Parsed responses with survey_id first and fetched_at last.
parse_pull <- function(pages, dict, def, survey_id) {
  parse_responses(pages, dict, sso = !is.null(def$sso)) |>
    dplyr::mutate(
      survey_id = as.integer(survey_id),
      fetched_at = Sys.time(),
      .before = 1
    ) |>
    dplyr::relocate("fetched_at", .after = dplyr::last_col())
}

# Sort by id, set the labels and the metadata attribute.
finish <- function(out, dict, def, survey_id, include_test, total_count) {
  restore_labels(out[order(out$id), ], dict) |>
    structure(
      alchemer = list(
        survey_id = as.integer(survey_id),
        title = chr(def$title),
        status = chr(def$status),
        modified_on = chr(def$modified_on),
        total_count = as.numeric(total_count),
        max_date_updated = if (nrow(out) > 0) {
          max(out$date_updated)
        } else {
          as.POSIXct(NA, tz = "America/Toronto")
        },
        include_test = include_test,
        schema_version = 1L,
        tz = "America/Toronto",
        package_version = as.character(utils::packageVersion("alchemeR")),
        dictionary = dict
      )
    )
}

# Incremental update of a local file (ADR-7, steps 4-7). Returns the merged
# data and the number of rows pulled, or a `reason` why a full pull is needed.
update_local <- function(
  local,
  def,
  survey_id,
  token,
  secret_key,
  include_test,
  full_refresh_days
) {
  meta <- attr(local, "alchemer")
  path <- paste0("survey/", survey_id, "/surveyresponse")
  ask <- function(filters, ...) {
    api_get(
      path,
      c(
        do.call(filter_query, c(base_filters(include_test), filters)),
        list(...)
      ),
      token,
      secret_key,
      simplify = FALSE
    )
  }

  # 1-row probe: how many responses, and when was the newest one updated?
  probe <- ask(list(), resultsperpage = 1, page = 1, order_by = "-date_updated")
  latest <- if (length(probe$data) > 0) {
    parse_time(chr(probe$data[[1]]$date_updated))
  } else {
    NA
  }
  if (
    as.numeric(probe$total_count) == nrow(local) &&
      identical(as.numeric(latest), as.numeric(meta$max_date_updated))
  ) {
    return(list(data = local, n_changed = 0))
  }

  # Everything updated since two days before the newest stored update.
  watermark <- meta$max_date_updated - 2 * 86400
  pages <- api_get_all(
    path,
    do.call(
      filter_query,
      c(
        base_filters(include_test),
        list(c("date_updated", ">=", utc_string(watermark)))
      )
    ),
    token = token,
    secret_key = secret_key,
    simplify = FALSE
  )
  new <- parse_pull(pages, dictionary_from_definition(def), def, survey_id)
  # Rows that are new or have a different update time than the stored copy.
  i <- match(new$id, local$id)
  n_changed <- sum(
    is.na(i) | new$date_updated != local$date_updated[i],
    na.rm = TRUE
  )
  stable <- !is.na(local$date_updated) &
    local$date_updated < watermark &
    !local$id %in% new$id
  merged <- dplyr::bind_rows(local[stable, ], new)
  if (anyDuplicated(merged$id) > 0) {
    return(list(reason = "duplicate response ids"))
  }

  # Older rows must still match the API (catches deletions outside the window).
  older <- ask(
    list(c("date_updated", "<", utc_string(watermark))),
    resultsperpage = 1,
    page = 1
  )
  if (
    as.numeric(older$total_count) !=
      sum(merged$date_updated < watermark, na.rm = TRUE)
  ) {
    return(list(reason = "older responses changed"))
  }

  # Safety net: pull everything again now and then, if the survey is active.
  if (
    n_changed > 0 &&
      difftime(Sys.time(), min(merged$fetched_at), units = "days") >
        full_refresh_days
  ) {
    return(list(
      reason = paste("the oldest rows are over", full_refresh_days, "days old")
    ))
  }

  list(
    data = finish(
      merged,
      meta$dictionary,
      def,
      survey_id,
      include_test,
      probe$total_count
    ),
    n_changed = n_changed
  )
}

# Row subsetting and binding can drop the label attributes; set them again.
restore_labels <- function(df, dict) {
  labels <- dict[!duplicated(dict$question_id), ]
  for (col in intersect(names(df), labels$question_id)) {
    attr(df[[col]], "label") <- labels$label[labels$question_id == col]
  }
  df
}

# Write to a temp file in the same folder, then rename: a failed write never
# damages the existing file. A failure is a warning, the data is still returned.
write_local <- function(df, path) {
  tmp <- tempfile(tmpdir = dirname(path), fileext = ".tmp")
  on.exit(unlink(tmp))
  tryCatch(
    {
      dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
      saveRDS(df, tmp)
      if (!suppressWarnings(file.rename(tmp, path))) {
        stop("could not replace ", path)
      }
    },
    error = \(e) {
      warning(
        "Could not save ",
        path,
        " (",
        conditionMessage(e),
        "); returning the data without saving.",
        call. = FALSE
      )
    }
  )
  invisible(df)
}

read_local <- function(path, survey_id) {
  df <- tryCatch(suppressWarnings(readRDS(path)), error = \(e) NULL)
  if (!identical(attr(df, "alchemer")$survey_id, as.integer(survey_id))) {
    stop(
      path,
      " is not a readable file for survey ",
      survey_id,
      "; use refresh = \"full\".",
      call. = FALSE
    )
  }
  df
}

format_seconds <- function(x) {
  if (x < 60) {
    sprintf("%.1f s", x)
  } else {
    sprintf("%d min %d s", x %/% 60, round(x %% 60))
  }
}
