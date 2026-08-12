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

# Strips a trailing EST/EDT-style abbreviation before casting -- date_updated
# carries no such suffix and needs none. Both remain naive timestamps; ADR-004
# already treats their timezone as ambiguous/configurable per account, and
# pub_layer() does not resolve that ambiguity, only removes what would
# otherwise make the cast fail outright.
timestamp_sql <- function(column) {
  glue::glue("TRY_CAST(regexp_replace({column}, ' [A-Z]{{2,5}}$', '') AS TIMESTAMP)")
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

rebuild_pub_surveys <- function(con, survey_ids) {
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
            {timestamp_sql('created_on')} AS created_on,
            {timestamp_sql('modified_on')} AS modified_on,
            team, is_deleted
     FROM {ducklake_alias}.raw.surveys
     WHERE survey_id IN ({ids})"
  ))
}

rebuild_pub_survey <- function(con, survey_id, language) {
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
            {timestamp_sql('date_submitted')} AS date_submitted,
            {timestamp_sql('date_started')} AS date_started,
            {timestamp_sql('date_updated')} AS date_updated,
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
#' @param db Application database directory. Defaults to [alchemer_db_path()].
#' @param surveys If supplied, only these survey ids are rebuilt. Otherwise
#'   every survey in `raw.surveys` is rebuilt.
#' @param language Which title language to resolve multilingual fields to.
#'   Letters, digits, spaces, and `-`/`_` only (Alchemer language names are
#'   always simple words, e.g. `"English"`) -- title_sql() interpolates this
#'   directly into a generated SQL/JSONPath expression with no further
#'   escaping, so it is validated up front rather than quoted in context.
#' @param wide_views Whether to (re)generate the per-survey wide views.
#' @return Invisibly, the character vector of survey ids rebuilt.
#' @export
pub_layer <- function(db = alchemer_db_path(), surveys = NULL, language = "English", wide_views = TRUE) {
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

  rebuild_pub_surveys(con, survey_ids)
  titles <- DBI::dbGetQuery(con, glue::glue(
    "SELECT survey_id, title FROM {ducklake_alias}.pub.surveys
     WHERE survey_id IN ({id_list_sql(con, survey_ids)})"
  ))

  for (survey_id in survey_ids) {
    rebuild_pub_survey(con, survey_id, language)
    if (wide_views) {
      title <- titles$title[titles$survey_id == survey_id][1] %||% survey_id
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
