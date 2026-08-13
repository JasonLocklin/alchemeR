# Builds the pub schema from raw alone (ADR-008): typed, language-resolved
# tables plus generated per-survey wide views. Rebuildable, incremental by
# survey, safe to re-run -- it is the only layer that does not need
# archival-grade fidelity, so it is where judgement calls (typing, language,
# reporting-value joins) belong, per ADR-003.

# A comma-separated, quoted SQL literal list, for `x IN (...)`.
id_list_sql <- function(con, ids) {
  paste(DBI::dbQuoteString(con, ids), collapse = ",")
}

# `title` on survey_questions/survey_question_options is a {"English": "..."}
# style map (ADR-003); survey titles themselves are plain strings, not maps.
# `language` is interpolated into both a SQL string literal and a JSONPath
# expression with no escaping here -- pub_layer() validates it against a
# strict character allowlist before it ever reaches this function.
title_sql <- function(column, language) {
  glue::glue("json_extract_string({column}, '$.\"{language}\"')")
}

# `raw` keeps every timestamp exactly as Alchemer sent it (ADR-003); `pub`
# presents them all as wall-clock time in one timezone, alchemer_tz()
# (`ALCHEMER_TZ`), so analysts never have to reason about mixed source
# shapes. Alchemer sends two shapes, needing two different treatments:
#
# - date_submitted/date_started carry an explicit, per-row EST or EDT
#   suffix -- the exact Eastern-time abbreviation for that date -- so they
#   parse into an unambiguous instant with no assumption of our own, and
#   then convert to the configured zone (tz_timestamp_sql). For an Eastern
#   ALCHEMER_TZ the result is numerically identical to the string Alchemer
#   sent, just parsed rigorously rather than string-munged; for any other
#   zone it is the correct local rendering of the same instant.
# - date_updated/created_on/modified_on carry no timezone suffix at all,
#   and are already in the account's own local time -- Alchemer's
#   documented single-response example makes this plain:
#     date_started   2025-12-09 17:15:30 EST
#     date_submitted 2025-12-09 17:15:37 EST
#     date_updated   2025-12-09 17:15:43       <- no suffix, same clock
#   6 seconds after submission, not 5 hours before it. So there is nothing
#   to convert: they are cast straight to TIMESTAMP (naive_timestamp_sql).
#   Reading them as UTC instead -- which Alchemer's docs describe as
#   "typically UTC, but may vary by account settings", while also warning
#   not to hardcode it -- shifted every one of them into the previous
#   evening and made date_updated < date_started for every response.
#
# This holds while the Alchemer account's timezone and ALCHEMER_TZ agree,
# which is the case they exist for. An account reporting in a *different*
# zone from the analysts reading `pub` would need a second knob for the
# source zone; raw.responses.date_updated stays verbatim either way.

# For date_submitted/date_started: parse using each row's own EST/EDT
# suffix into an exact instant, then render it in `tz`.
est_edt_timestamp_sql <- function(column, tz) {
  glue::glue(
    "(CASE
        WHEN {column} LIKE '% EST' THEN
          TRY_CAST(regexp_replace({column}, ' EST$', '') || '-05' AS TIMESTAMPTZ)
        WHEN {column} LIKE '% EDT' THEN
          TRY_CAST(regexp_replace({column}, ' EDT$', '') || '-04' AS TIMESTAMPTZ)
        ELSE NULL
      END) AT TIME ZONE {DBI::dbQuoteString(DBI::ANSI(), tz)}"
  )
}

# For date_updated/created_on/modified_on: already account-local wall clock
# (see the comment above), so cast and leave the reading alone.
naive_timestamp_sql <- function(column) {
  glue::glue("TRY_CAST({column} AS TIMESTAMP)")
}

# ASCII, lowercase, underscore-separated, length-capped -- for both the wide
# view's slug and its per-question column aliases (ADR-008 / §9 of the spec).
slugify <- function(x, max_len = 40) {
  x <- iconv(x, to = "ASCII//TRANSLIT")
  x <- tolower(ifelse(is.na(x), "", x))
  x <- gsub("[^a-z0-9]+", "_", x)
  x <- gsub("^_+|_+$", "", x)
  x <- substr(x, 1, max_len)
  ifelse(nzchar(x), x, "x")
}

rebuild_pub_surveys <- function(con, survey_ids, tz) {
  ids <- id_list_sql(con, survey_ids)
  DBI::dbExecute(con, glue::glue(
    "DELETE FROM {ducklake_alias}.pub.surveys WHERE survey_id IN ({ids})"
  ))
  # BY NAME (not a plain positional INSERT ... SELECT) matches each SELECT
  # column to its target by the alias given here, not by position -- so a
  # future column added/reordered in pub_tables' DDL (db_schema.R) without a
  # matching update to this SELECT list fails loudly on a name mismatch (or,
  # for a genuinely new target column with no matching alias, inserts NULL)
  # instead of silently writing a value into the wrong column. Every INSERT
  # in this file uses the same pattern.
  DBI::dbExecute(con, glue::glue(
    "INSERT INTO {ducklake_alias}.pub.surveys BY NAME
     SELECT survey_id, title, type, status,
            {naive_timestamp_sql('created_on')} AS created_on,
            {naive_timestamp_sql('modified_on')} AS modified_on,
            team, is_deleted
     FROM {ducklake_alias}.raw.surveys
     WHERE survey_id IN ({ids})"
  ))
}

rebuild_pub_survey <- function(con, survey_id, language, tz) {
  qs <- DBI::dbQuoteString(con, survey_id)

  DBI::dbExecute(con, glue::glue("DELETE FROM {ducklake_alias}.pub.questions WHERE survey_id = {qs}"))
  DBI::dbExecute(con, glue::glue(
    "INSERT INTO {ducklake_alias}.pub.questions BY NAME
     SELECT survey_id, question_id, page_id, type,
            {title_sql('title', language)} AS title,
            shortname, varname, question_order
     FROM {ducklake_alias}.raw.survey_questions WHERE survey_id = {qs}"
  ))

  DBI::dbExecute(con, glue::glue("DELETE FROM {ducklake_alias}.pub.options WHERE survey_id = {qs}"))
  DBI::dbExecute(con, glue::glue(
    "INSERT INTO {ducklake_alias}.pub.options BY NAME
     SELECT survey_id, question_id, option_id,
            {title_sql('title', language)} AS title,
            value, option_order
     FROM {ducklake_alias}.raw.survey_question_options WHERE survey_id = {qs}"
  ))

  DBI::dbExecute(con, glue::glue("DELETE FROM {ducklake_alias}.pub.responses WHERE survey_id = {qs}"))
  DBI::dbExecute(con, glue::glue(
    "INSERT INTO {ducklake_alias}.pub.responses BY NAME
     SELECT survey_id, response_id, status,
            (is_test_data = '1') AS is_test_data,
            {est_edt_timestamp_sql('date_submitted', tz)} AS date_submitted,
            {est_edt_timestamp_sql('date_started', tz)} AS date_started,
            {naive_timestamp_sql('date_updated')} AS date_updated,
            session_id, language, link_id, contact_id,
            TRY_CAST(response_time AS INTEGER) AS response_time,
            is_deleted
     FROM {ducklake_alias}.raw.responses WHERE survey_id = {qs}"
  ))

  # Reporting value / option_id are recovered on a best-effort basis by
  # matching the verbatim answer text against an option's resolved title --
  # Alchemer's per-question-type answer shape varies too much (ADR-003) to
  # do better universally. Analysts needing more can always fall back to
  # raw.responses.survey_data, which is untouched by this join.
  DBI::dbExecute(con, glue::glue("DELETE FROM {ducklake_alias}.pub.answers WHERE survey_id = {qs}"))
  DBI::dbExecute(con, glue::glue(
    "INSERT INTO {ducklake_alias}.pub.answers BY NAME
     SELECT
       r.survey_id, r.response_id, je.key AS question_id,
       (SELECT q.shortname FROM {ducklake_alias}.raw.survey_questions q
          WHERE q.survey_id = r.survey_id AND q.question_id = je.key LIMIT 1) AS question_shortname,
       (SELECT {title_sql('q.title', language)} FROM {ducklake_alias}.raw.survey_questions q
          WHERE q.survey_id = r.survey_id AND q.question_id = je.key LIMIT 1) AS question_title,
       (SELECT o.option_id FROM {ducklake_alias}.raw.survey_question_options o
          WHERE o.survey_id = r.survey_id AND o.question_id = je.key
            AND {title_sql('o.title', language)} = json_extract_string(je.value, '$.answer')
          LIMIT 1) AS option_id,
       json_extract_string(je.value, '$.answer') AS answer,
       COALESCE(
         (SELECT o.value FROM {ducklake_alias}.raw.survey_question_options o
            WHERE o.survey_id = r.survey_id AND o.question_id = je.key
              AND {title_sql('o.title', language)} = json_extract_string(je.value, '$.answer')
            LIMIT 1),
         json_extract_string(je.value, '$.answer')
       ) AS reporting_value,
       TRY_CAST(json_extract_string(je.value, '$.shown') AS BOOLEAN) AS shown,
       r.is_deleted
     FROM {ducklake_alias}.raw.responses r, json_each(r.survey_data) je
     WHERE r.survey_id = {qs}"
  ))
}

# Column aliases for the wide pivot: shortname where present and unique,
# "q<id>" otherwise, deduplicated by suffixing the question_id (the
# duplicate-shortname collision rule ADR-008 requires be documented).
pivot_aliases <- function(question_ids, shortnames) {
  base <- ifelse(is.na(shortnames) | !nzchar(shortnames), paste0("q", question_ids), shortnames)
  base <- slugify(base, max_len = 60)
  dupe <- duplicated(base) | duplicated(base, fromLast = TRUE)
  base[dupe] <- paste0(base[dupe], "_", question_ids[dupe])
  base
}

# Escapes a value for safe embedding inside a LIKE pattern: %, _, and \\
# itself all need a \\ in front so they're matched literally rather than as
# wildcards/the escape character.
like_escape <- function(x) {
  gsub("([%_\\\\])", "\\\\\\1", x)
}

drop_existing_wide_views <- function(con, survey_id) {
  # Unlike every other use of survey_id in this file, this one used to
  # interpolate it directly into the SQL string literal with no quoting at
  # all -- a survey_id containing a quote character would break out of the
  # literal. dbQuoteString() handles that; like_escape() separately handles
  # survey_id containing a genuine LIKE wildcard character.
  pattern <- paste0("wide\\_%\\_", like_escape(survey_id))
  existing <- DBI::dbGetQuery(con, glue::glue(
    "SELECT table_name FROM information_schema.tables
     WHERE table_catalog = '{ducklake_alias}' AND table_schema = 'pub'
       AND table_name LIKE {DBI::dbQuoteString(con, pattern)} ESCAPE '\\'"
  ))$table_name
  for (view in existing) {
    DBI::dbExecute(con, glue::glue(
      "DROP VIEW IF EXISTS {ducklake_alias}.pub.{DBI::dbQuoteIdentifier(con, view)}"
    ))
  }
}

rebuild_wide_view <- function(con, survey_id, title) {
  drop_existing_wide_views(con, survey_id)

  questions <- DBI::dbGetQuery(con, glue::glue(
    "SELECT question_id, shortname FROM {ducklake_alias}.pub.questions
     WHERE survey_id = {DBI::dbQuoteString(con, survey_id)} ORDER BY question_order"
  ))
  if (nrow(questions) == 0) {
    return(invisible(NULL))
  }

  aliases <- pivot_aliases(questions$question_id, questions$shortname)
  pivot_list <- paste(
    DBI::dbQuoteString(con, questions$question_id), "AS", DBI::dbQuoteIdentifier(con, aliases),
    collapse = ", "
  )
  # view_name becomes a raw SQL identifier below (a table/view name can't be
  # a bind parameter), so it goes through dbQuoteIdentifier() even though
  # survey_id is almost always numeric in practice -- raw.surveys.survey_id
  # is untyped VARCHAR (ADR-003) and pub_layer(surveys = ) accepts whatever
  # the caller passes.
  view_name <- paste0("wide_", slugify(title), "_", survey_id)

  DBI::dbExecute(con, glue::glue(
    "CREATE VIEW {ducklake_alias}.pub.{DBI::dbQuoteIdentifier(con, view_name)} AS
     PIVOT (SELECT * FROM {ducklake_alias}.pub.answers WHERE survey_id = {DBI::dbQuoteString(con, survey_id)})
     ON question_id IN ({pivot_list})
     USING first(answer)
     GROUP BY response_id"
  ))
}

#' Build the publication layer from the raw layer
#'
#' Typed, English-resolved (or `language`-resolved) tables for ordinary
#' analysis, plus one generated wide view per survey. Rebuildable from `raw`
#' alone, incremental by survey, and safe to re-run (ADR-008) -- it does not
#' need archival-grade fidelity, so this is where judgement calls (typing,
#' language selection, reporting-value joins) belong instead of in `ingest()`.
#'
#' Every timestamp in `pub` (`pub.surveys.created_on`/`modified_on`,
#' `pub.responses.date_submitted`/`date_started`/`date_updated`) is presented
#' as wall-clock time in `tz`. `date_submitted`/`date_started` carry an
#' explicit per-row EST/EDT suffix from Alchemer, so they are parsed into an
#' exact instant and rendered in `tz`; `date_updated` and the two
#' survey-level fields carry no suffix and are already in the account's own
#' local time, so they are cast unchanged. `raw` is never converted at all.
#' See `vignette("data-model")`.
#'
#' @param db Application database directory. Defaults to [alchemer_db_path()].
#' @param surveys If supplied, only these survey ids are rebuilt. Otherwise
#'   every survey in `raw.surveys` is rebuilt.
#' @param language Which title language to resolve multilingual fields to.
#'   Letters, digits, spaces, and `-`/`_` only (Alchemer language names are
#'   always simple words, e.g. `"English"`) -- title_sql() interpolates this
#'   directly into a generated SQL/JSONPath expression with no further
#'   escaping, so it is validated up front rather than quoted in context.
#' @param wide_views Whether to (re)generate the per-survey wide views.
#' @param tz Timezone for every `pub` timestamp. Defaults to `ALCHEMER_TZ`,
#'   then the machine's own timezone (see [alchemer_tz()]). Set `ALCHEMER_TZ`
#'   explicitly for scheduled runs so the values written don't depend on
#'   which machine ran them.
#' @return Invisibly, the character vector of survey ids rebuilt.
#' @export
pub_layer <- function(db = alchemer_db_path(), surveys = NULL, language = "English",
                      wide_views = TRUE, tz = NULL) {
  # Routed through alchemer_tz() even when supplied directly, so an explicit
  # argument is validated against the IANA database exactly like the
  # environment variable is -- `tz` reaches SQL as a bare literal.
  tz <- alchemer_tz(tz)
  if (!grepl("^[A-Za-z0-9 _-]+$", language)) {
    cli::cli_abort(
      "`language` must contain only letters, digits, spaces, '-', or '_'; got {.val {language}}.",
      class = "alchemeR_config_error"
    )
  }
  con <- alchemer_db(db)
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))

  survey_ids <- if (!is.null(surveys)) {
    as.character(surveys)
  } else {
    DBI::dbGetQuery(con, glue::glue("SELECT survey_id FROM {ducklake_alias}.raw.surveys"))$survey_id
  }
  if (length(survey_ids) == 0) {
    return(invisible(character(0)))
  }

  rebuild_pub_surveys(con, survey_ids, tz)
  titles <- DBI::dbGetQuery(con, glue::glue(
    "SELECT survey_id, title FROM {ducklake_alias}.pub.surveys
     WHERE survey_id IN ({id_list_sql(con, survey_ids)})"
  ))

  for (survey_id in survey_ids) {
    rebuild_pub_survey(con, survey_id, language, tz)
    if (wide_views) {
      # or_default(), not %||%: a survey with no title, or an id that isn't
      # in raw.surveys at all, gives NA here rather than NULL, which %||%
      # does not catch -- and slugify(NA) yields the placeholder "x", so the
      # view came out named `wide_x_<id>` instead of falling back to the id.
      title <- or_default(titles$title[titles$survey_id == survey_id][1], survey_id)
      rebuild_wide_view(con, survey_id, title)
    }
  }

  invisible(survey_ids)
}

#' Pivot one survey's answers to one row per respondent, on demand
#'
#' The same shape as `pub.wide_*` views, computed directly for a survey
#' that hasn't been through [pub_layer()] (or whose view is stale).
#'
#' @param con A connection from [alchemer_db()].
#' @param survey_id Survey id.
#' @return A tibble, one row per respondent.
#' @export
survey_wide <- function(con, survey_id) {
  questions <- DBI::dbGetQuery(con, glue::glue(
    "SELECT question_id, shortname FROM {ducklake_alias}.raw.survey_questions
     WHERE survey_id = {DBI::dbQuoteString(con, survey_id)} ORDER BY question_order"
  ))
  if (nrow(questions) == 0) {
    return(tibble::tibble())
  }
  aliases <- pivot_aliases(questions$question_id, questions$shortname)
  pivot_list <- paste(
    DBI::dbQuoteString(con, questions$question_id), "AS", DBI::dbQuoteIdentifier(con, aliases),
    collapse = ", "
  )

  answers <- DBI::dbGetQuery(con, glue::glue(
    "SELECT r.survey_id, r.response_id, je.key AS question_id,
            json_extract_string(je.value, '$.answer') AS answer
     FROM {ducklake_alias}.raw.responses r, json_each(r.survey_data) je
     WHERE r.survey_id = {DBI::dbQuoteString(con, survey_id)}"
  ))
  duckdb::duckdb_register(con, "tmp_survey_wide_answers", answers)
  on.exit(duckdb::duckdb_unregister(con, "tmp_survey_wide_answers"), add = TRUE)

  tibble::as_tibble(DBI::dbGetQuery(con, glue::glue(
    "PIVOT tmp_survey_wide_answers ON question_id IN ({pivot_list}) USING first(answer) GROUP BY response_id"
  )))
}
