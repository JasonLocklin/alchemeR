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
#   then convert to the configured zone (est_edt_timestamp_sql). For an Eastern
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

# What this run was built from and with, committed alongside the rows it
# describes (see rebuild_survey_atomically()).
record_pub_state <- function(con, survey_id, plan_row, language, tz, wide_views, now) {
  DBI::dbExecute(con, glue::glue(
    "DELETE FROM {ducklake_alias}.meta.pub_state
     WHERE survey_id = {DBI::dbQuoteString(con, survey_id)}"
  ))
  write_rows_generic(con, "meta.pub_state", tibble::tibble(
    survey_id = survey_id, built_at = now,
    responses_watermark = plan_row$responses_watermark,
    definition_watermark = plan_row$definition_watermark,
    n_responses = plan_row$n_responses, n_definition = plan_row$n_definition,
    language = language, tz = tz, wide_views = isTRUE(wide_views)
  ))
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

# Rebuild one survey's `pub` rows.
#
# `since` is the watermark from the last successful build of this survey, or
# NULL to rebuild the survey wholesale. When it is supplied, only the responses
# whose raw row has been written or flagged since then are touched -- which is
# what makes a run that finds five new responses in one survey write five rows
# rather than the survey's entire answer set. See pub_watermarks() for why the
# question/option tables force a full rebuild instead.
rebuild_pub_survey <- function(con, survey_id, language, tz, since = NULL) {
  qs <- DBI::dbQuoteString(con, survey_id)

  if (!is.null(since)) {
    return(rebuild_pub_survey_rows(con, survey_id, language, tz, since))
  }

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
  DBI::dbExecute(con, pub_responses_insert_sql(con, tz, glue::glue("survey_id = {qs}")))

  DBI::dbExecute(con, glue::glue("DELETE FROM {ducklake_alias}.pub.answers WHERE survey_id = {qs}"))
  DBI::dbExecute(con, pub_answers_insert_sql(con, language, glue::glue("r.survey_id = {qs}")))
}

# The two response-scoped INSERTs, factored out so the wholesale rebuild above
# and the incremental one below issue exactly the same SQL with a different
# `where` -- there is no second copy of the typing rules to drift.
pub_responses_insert_sql <- function(con, tz, where) {
  glue::glue(
    "INSERT INTO {ducklake_alias}.pub.responses BY NAME
     SELECT survey_id, response_id, status,
            (is_test_data = '1') AS is_test_data,
            {est_edt_timestamp_sql('date_submitted', tz)} AS date_submitted,
            {est_edt_timestamp_sql('date_started', tz)} AS date_started,
            {naive_timestamp_sql('date_updated')} AS date_updated,
            session_id, language, link_id, contact_id,
            TRY_CAST(response_time AS INTEGER) AS response_time,
            is_deleted
     FROM {ducklake_alias}.raw.responses WHERE {where}"
  )
}

# Reporting value / option_id are recovered on a best-effort basis by
# matching the verbatim answer text against an option's resolved title --
# Alchemer's per-question-type answer shape varies too much (ADR-003) to
# do better universally. Analysts needing more can always fall back to
# raw.responses.survey_data, which is untouched by this join.
pub_answers_insert_sql <- function(con, language, where) {
  glue::glue(
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
     WHERE {where}"
  )
}

# The incremental half of rebuild_pub_survey(): only the responses whose raw
# row was written or flagged after `since`.
#
# `ingested_at` is the right signal and the merge in db_schema.R is what makes
# it one: a row that Alchemer returned unchanged is not rewritten, so it keeps
# the `ingested_at` of the run that last actually changed it, while every new
# or edited row carries the current run's. A response that vanished upstream is
# not rewritten either -- it is flagged in place -- so `deleted_detected_at`
# has to be checked alongside it, or a deletion would never reach `pub`.
#
# The comparison is strictly `>`. The stored watermark is the maximum
# `ingested_at` the last build *saw*, so every row at exactly that timestamp
# was already built; `>=` would rewrite the whole of the previous run's batch
# on every run, which for a survey taking 15,000 responses a day is 15,000
# rows of pointless churn. Nothing is lost at the boundary because a refresh
# is atomic per survey (refresh_survey() wraps one survey in one transaction):
# either its rows were visible when the watermark was taken -- in which case
# they are in the watermark and were built -- or they were not, in which case
# the watermark predates them and this clause catches them next run.
rebuild_pub_survey_rows <- function(con, survey_id, language, tz, since) {
  qs <- DBI::dbQuoteString(con, survey_id)
  # Read the watermark from meta.pub_state rather than formatting `since` (a
  # POSIXct) as a literal. POSIXct is a double (seconds since epoch); at
  # 2026-era timestamps the precision is ~0.4 µs, so format("%OS6") can
  # produce a literal 1 µs earlier than the stored int64 value -- making rows
  # that exactly equal the watermark satisfy ">", and rebuilding more than
  # changed. The subquery keeps the comparison in DuckDB's own int64 type.
  since_in_db <- glue::glue(
    "(SELECT responses_watermark FROM {ducklake_alias}.meta.pub_state WHERE survey_id = {qs})"
  )
  changed <- glue::glue(
    "(ingested_at > {since_in_db} OR deleted_detected_at > {since_in_db})"
  )
  changed_ids <- glue::glue(
    "SELECT response_id FROM {ducklake_alias}.raw.responses
     WHERE survey_id = {qs} AND {changed}"
  )

  DBI::dbExecute(con, glue::glue(
    "DELETE FROM {ducklake_alias}.pub.responses
     WHERE survey_id = {qs} AND response_id IN ({changed_ids})"
  ))
  DBI::dbExecute(con, pub_responses_insert_sql(
    con, tz, glue::glue("survey_id = {qs} AND {changed}")
  ))

  DBI::dbExecute(con, glue::glue(
    "DELETE FROM {ducklake_alias}.pub.answers
     WHERE survey_id = {qs} AND response_id IN ({changed_ids})"
  ))
  DBI::dbExecute(con, pub_answers_insert_sql(
    con, language,
    glue::glue("r.survey_id = {qs} AND {changed}")
  ))

  # A response removed from `raw` outright -- which only expunge() does, and
  # which it already cleans up in `pub` itself -- leaves nothing behind to
  # match on, so it is not handled here. Upstream deletions are flagged, not
  # removed (ADR-005), and are caught by the deleted_detected_at clause above.
  invisible(NULL)
}

# One survey's rebuild, its wide view, and the record of the build, in a single
# transaction. Two reasons, in that order of importance:
#
# - The meta.pub_state row must commit with the rows it describes. Written in a
#   separate transaction, a failure between the two would leave a survey marked
#   as built from data that had just been rolled back, and every later run would
#   then skip it: stale forever, and silently. This is the one ordering in the
#   incremental path that can actually corrupt the result.
# - It is also much faster. Every statement outside a transaction is its own
#   DuckLake snapshot, and a snapshot costs a catalog write regardless of how
#   many rows it carries -- which is why rebuilding a survey's 20 question rows
#   cost about as much as its 10,000 answer rows. *Measured* on a 50-survey,
#   27k-response archive, grouping each survey's statements into one
#   transaction took a full rebuild from 778 snapshots and 43s to 128 and 25s.
#
# ROLLBACK runs from on.exit() rather than a tryCatch(), so an error still
# propagates out of pub_layer() exactly as it did before -- this changes what a
# failure leaves behind, not whether it is reported.
rebuild_survey_atomically <- function(con, survey_id, title, language, tz, wide_views,
                                      plan_row, now = Sys.time()) {
  committed <- FALSE
  DBI::dbExecute(con, "BEGIN")
  on.exit(if (!committed) try(DBI::dbExecute(con, "ROLLBACK"), silent = TRUE), add = TRUE)

  if (identical(plan_row$action, "full")) {
    rebuild_pub_survey(con, survey_id, language, tz)
    if (wide_views) rebuild_wide_view(con, survey_id, title)
  } else {
    rebuild_pub_survey(con, survey_id, language, tz,
                       since = plan_row$responses_watermark_prior)
  }
  record_pub_state(con, survey_id, plan_row, language, tz, wide_views, now)

  DBI::dbExecute(con, "COMMIT")
  committed <- TRUE
  invisible(NULL)
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
#' A survey is rebuilt only when something it is built from or with has
#' changed (ADR-017): the `raw` rows themselves, or the `language`, `tz`, or
#' `wide_views` setting, or the version of alchemeR doing the building. A run
#' where nothing changed upstream does no per-survey work at all, which is what
#' makes this cheap enough to call on every pipeline tick. Each survey that is
#' rebuilt is rebuilt whole, in one transaction, exactly as before -- there is
#' no partial or row-level update of `pub`.
#'
#' `meta.pub_state` records what each survey was last built from. Deleting a
#' row from it, or passing `force = TRUE`, forces that survey to be rebuilt on
#' the next run.
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
#' @param surveys If supplied, only these survey ids are considered, and each
#'   is rebuilt whether or not it has changed. Otherwise every survey in
#'   `raw.surveys` is considered and the changed ones are rebuilt.
#' @param wide_views Whether to (re)generate the per-survey wide views.
#' @param force Rebuild every survey considered, skipping change detection.
#'   For rebuilding after something outside this function has touched `pub`.
#' @section Configuration:
#' The timezone and the title language come from the environment and cannot be
#' passed here (ADR-019): `ALCHEMER_TZ` and `ALCHEMER_LANGUAGE`. Changing
#' either makes the next run rebuild every survey, since both change the values
#' written from identical input. Set `ALCHEMER_TZ` explicitly for a scheduled
#' run — unset, it falls back to the machine's own timezone, so a laptop and a
#' UTC server would write different values into one database.
#' @return Invisibly, the character vector of survey ids actually rebuilt --
#'   which on an unchanged database is legitimately empty.
#' @seealso [ingest()], which fills the `raw` layer this is built from.
#' @export
pub_layer <- function(db = alchemer_db_path(), surveys = NULL,
                      wide_views = TRUE, force = FALSE) {
  # Both validated by their config readers before reaching SQL, where each
  # arrives as a bare literal: an IANA name for the timezone, and a strict
  # character allowlist for the language (title_sql() interpolates it into a
  # generated SQL/JSONPath expression with no further escaping).
  tz <- alchemer_tz()
  language <- alchemer_language()
  con <- alchemer_db(db)
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  assert_schema_compatible(con, db)

  # A publication-layer schema change -- a bump of the schema *minor* version
  # (ADR-018) -- means `pub`'s tables, or the SQL that fills them, are not the
  # ones the stored rows were built with. Everything in `pub` is dropped,
  # recreated from the current DDL, and rebuilt below from `raw`, which is what
  # keeps `pub` from drifting: CREATE TABLE IF NOT EXISTS would otherwise leave
  # a table created by an older version with its old columns forever.
  #
  # The new version is stamped in the same transaction as the reset, and before
  # any survey is rebuilt, so this is resumable: if the rebuild that follows is
  # interrupted, the next run doesn't reset again -- it finds an empty
  # meta.pub_state and simply builds the surveys that didn't get done.
  stamped <- db_schema_version(con)
  if (!is.null(stamped) && !identical(stamped$minor, schema_minor)) {
    DBI::dbExecute(con, "BEGIN")
    reset_pub_schema(con)
    stamp_schema_version(con, schema_major, schema_minor)
    DBI::dbExecute(con, "COMMIT")
  }

  survey_ids <- if (!is.null(surveys)) {
    as.character(surveys)
  } else {
    DBI::dbGetQuery(con, glue::glue("SELECT survey_id FROM {ducklake_alias}.raw.surveys"))$survey_id
  }
  if (length(survey_ids) == 0) {
    return(invisible(character(0)))
  }

  # An explicit `surveys =` is a request to rebuild those surveys, so it
  # implies force for them -- the caller has named them for a reason, and
  # silently skipping one because nothing changed would be surprising.
  plan <- pub_plan(con, survey_ids, language, tz, wide_views = isTRUE(wide_views),
                   force = isTRUE(force) || !is.null(surveys))
  to_rebuild <- plan[plan$action != "skip", , drop = FALSE]

  # pub.surveys is rebuilt unconditionally, for every survey considered rather
  # than only the changed ones. It is one statement over one small row per
  # survey (*measured*: 0.17s for 50 surveys, against 25s+ for their per-survey
  # rebuilds), and keeping it outside the incremental path means survey-level
  # facts -- a retitling, a status change, an is_deleted flag -- can never be
  # the thing that goes stale while the expensive tables are skipped.
  DBI::dbExecute(con, "BEGIN")
  rebuild_pub_surveys(con, survey_ids, tz)
  DBI::dbExecute(con, "COMMIT")

  if (nrow(to_rebuild) == 0) {
    return(invisible(character(0)))
  }

  titles <- DBI::dbGetQuery(con, glue::glue(
    "SELECT survey_id, title FROM {ducklake_alias}.pub.surveys
     WHERE survey_id IN ({id_list_sql(con, to_rebuild$survey_id)})"
  ))
  now <- Sys.time()

  for (i in seq_len(nrow(to_rebuild))) {
    survey_id <- to_rebuild$survey_id[i]
    # or_default(), not %||%: a survey with no title, or an id that isn't
    # in raw.surveys at all, gives NA here rather than NULL, which %||%
    # does not catch -- and slugify(NA) yields the placeholder "x", so the
    # view came out named `wide_x_<id>` instead of falling back to the id.
    title <- or_default(titles$title[titles$survey_id == survey_id][1], survey_id)
    rebuild_survey_atomically(
      con, survey_id, title, language, tz, wide_views,
      to_rebuild[i, , drop = FALSE], now
    )
  }

  invisible(to_rebuild$survey_id)
}

# What each survey needs this run, from the watermarks stored by the last one.
#
#   "skip"  nothing in `raw` has changed for it since the last build
#   "rows"  only responses changed: rebuild those responses' pub rows
#   "full"  the survey's questions, options or own record changed, or it has
#           never been built, or `language`/`tz`/`wide_views` differ from
#           the build stored
#
# The split matters because question titles and option values are joined into
# *every* answer row, so a definition change invalidates the whole survey while
# a batch of new responses invalidates only itself.
pub_plan <- function(con, survey_ids, language, tz, wide_views = TRUE, force = FALSE) {
  wm <- pub_watermarks(con)
  prior <- if (table_exists(con, "meta", "pub_state")) {
    DBI::dbGetQuery(con, glue::glue("SELECT * FROM {ducklake_alias}.meta.pub_state"))
  } else {
    data.frame()
  }

  out <- data.frame(survey_id = as.character(survey_ids), stringsAsFactors = FALSE)
  match_wm <- match(out$survey_id, wm$survey_id)
  out$responses_watermark <- wm$responses_wm[match_wm]
  out$definition_watermark <- wm$definition_wm[match_wm]
  out$n_responses <- as.integer(wm$responses_n[match_wm])
  out$n_definition <- as.integer(wm$definition_n[match_wm])

  has_prior <- nrow(prior) > 0
  match_prior <- if (has_prior) match(out$survey_id, prior$survey_id) else rep(NA_integer_, nrow(out))
  out$responses_watermark_prior <- if (has_prior) prior$responses_watermark[match_prior] else as.POSIXct(NA)
  definition_prior <- if (has_prior) prior$definition_watermark[match_prior] else as.POSIXct(NA)
  n_responses_prior <- if (has_prior) as.integer(prior$n_responses[match_prior]) else NA_integer_
  n_definition_prior <- if (has_prior) as.integer(prior$n_definition[match_prior]) else NA_integer_
  language_prior <- if (has_prior) prior$language[match_prior] else NA_character_
  tz_prior <- if (has_prior) prior$tz[match_prior] else NA_character_
  wide_views_prior <- if (has_prior && "wide_views" %in% names(prior)) {
    as.logical(prior$wide_views[match_prior])
  } else {
    NA
  }

  # newer(a, b): "a is strictly after b, or there is a watermark now where
  # there was none before". A survey with no watermark at all -- no rows, or
  # rows hand-inserted without `ingested_at`, which ingest() never does --
  # reads as unchanged, and is covered by never_built on its first build.
  newer <- function(now, before) !is.na(now) & (is.na(before) | now > before)

  never_built <- is.na(match_prior)
  settings_changed <- !never_built &
    (is.na(language_prior) | language_prior != language |
     is.na(tz_prior) | tz_prior != tz |
     is.na(wide_views_prior) | wide_views_prior != isTRUE(wide_views))
  # A row *count* that moved without its watermark moving means rows were
  # removed, which no incremental path can repair -- so it forces a full
  # rebuild rather than a row-level one. An unknown count on either side is
  # read as "no evidence of a removal", the same way an unknown watermark is.
  differ <- function(now, before) !is.na(now) & !is.na(before) & now != before
  fewer <- function(now, before) !is.na(now) & !is.na(before) & now < before

  definition_changed <- newer(out$definition_watermark, definition_prior) |
    differ(out$n_definition, n_definition_prior)
  responses_removed <- fewer(out$n_responses, n_responses_prior)
  responses_changed <- newer(out$responses_watermark, out$responses_watermark_prior)

  out$action <- ifelse(
    force | never_built | settings_changed | definition_changed | responses_removed |
      (responses_changed & is.na(out$responses_watermark_prior)), "full",
    ifelse(responses_changed, "rows", "skip")
  )
  out
}

# The state of `raw` per survey, as the two halves the plan reacts to
# differently, each as a (high-water mark, row count) pair.
#
# `deleted_detected_at` is checked alongside `ingested_at` because flagging a
# vanished row is an UPDATE that does not restamp `ingested_at` -- see
# rebuild_pub_survey_rows().
#
# The counts are not redundant with the watermarks: a *removed* row moves no
# watermark at all. Questions and options that vanish upstream are deleted
# outright rather than flagged (merge_survey_rows()'s `on_vanished = "delete"`),
# so without the count a deleted question would sit in `pub.questions` forever.
pub_watermarks <- function(con) {
  DBI::dbGetQuery(con, glue::glue(
    "WITH r AS (
       SELECT survey_id,
              MAX(GREATEST(ingested_at, COALESCE(deleted_detected_at, ingested_at))) AS wm,
              COUNT(*) AS n
       FROM {ducklake_alias}.raw.responses GROUP BY survey_id
     ), d AS (
       SELECT survey_id, MAX(wm) AS wm, COUNT(*) AS n FROM (
         SELECT survey_id,
                GREATEST(ingested_at, COALESCE(deleted_detected_at, ingested_at)) AS wm
           FROM {ducklake_alias}.raw.surveys
         UNION ALL SELECT survey_id, ingested_at FROM {ducklake_alias}.raw.survey_questions
         UNION ALL SELECT survey_id, ingested_at FROM {ducklake_alias}.raw.survey_question_options
       ) GROUP BY survey_id
     )
     SELECT s.survey_id,
            r.wm AS responses_wm, COALESCE(r.n, 0) AS responses_n,
            d.wm AS definition_wm, COALESCE(d.n, 0) AS definition_n
     FROM {ducklake_alias}.raw.surveys s
     LEFT JOIN r ON r.survey_id = s.survey_id
     LEFT JOIN d ON d.survey_id = s.survey_id"
  ))
}

#' Pivot one survey's answers to one row per respondent, on demand
#'
#' The same shape as `pub.wide_*` views, computed directly for a survey
#' that hasn't been through [pub_layer()] (or whose view is stale).
#'
#' @param con A `DBI` connection with the catalog attached as `alchemer` --
#'   see `vignette("getting-started")` for how to open one.
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
