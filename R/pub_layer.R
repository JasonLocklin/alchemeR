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

# --- Deciding what actually needs rebuilding (ADR-017) ----------------------
#
# A survey's watermark is the change signals of the four `raw` tables
# pub_layer() reads, concatenated. It is compared against the one recorded when
# the survey was last built; equal means nothing it is built from has moved,
# so the rebuild is skipped.
#
# Three signals per table, because there are three ways a row can change and no
# single column catches all of them:
#
#   max(ingested_at)         inserts and edits. ingest() merges rather than
#                            replaces (ADR-013), so this advances only for rows
#                            that actually changed -- the property asserted by
#                            "re-refreshing unchanged data rewrites nothing" in
#                            test-ingest.R. Without that merge behaviour this
#                            column would advance every run and the watermark
#                            would never match.
#   max(deleted_detected_at) a row flagged deleted. Flagging sets only
#                            is_deleted/deleted_detected_at and leaves
#                            ingested_at untouched, so max(ingested_at) alone
#                            would miss a deletion entirely -- the one case
#                            that would silently serve stale data.
#   count(*)                 a row removed outright, which is what
#                            on_vanished = "delete" does to question and option
#                            rows, and what expunge() does to responses.
#
# survey_questions/survey_question_options carry no deleted_detected_at column;
# their vanished rows are deleted rather than flagged, which count(*) catches.
#
# raw.surveys additionally contributes its columns *by value* (`v`), not just
# its bookkeeping. It is one row per survey, so reading the actual title,
# status, type, modified_on, and is_deleted costs nothing measurable, and it
# removes this table's dependence on ingested_at being stamped correctly --
# which matters because the survey title is what the wide view is *named*
# after, so a missed retitle leaves a view under a name that no longer exists
# in pub.surveys. Doing the same for the other three tables would mean reading
# every response and answer to decide whether to read every response and
# answer, which is the work this whole mechanism exists to avoid.
#
# Every component is COALESCEd to '' rather than left NULL, because concat_ws()
# *skips* NULL arguments: a NULL would shift every later component one place
# left, letting two genuinely different watermarks collide.
pub_source_watermarks <- function(con) {
  DBI::dbGetQuery(con, glue::glue("
    WITH s AS (
      SELECT survey_id,
             COALESCE(max(ingested_at)::VARCHAR, '') AS m,
             COALESCE(max(deleted_detected_at)::VARCHAR, '') AS d,
             count(*)::VARCHAR AS n,
             concat_ws('~', COALESCE(max(title), ''), COALESCE(max(status), ''),
                       COALESCE(max(type), ''), COALESCE(max(modified_on), ''),
                       COALESCE(max(is_deleted)::VARCHAR, '')) AS v
      FROM {ducklake_alias}.raw.surveys GROUP BY survey_id
    ),
    q AS (
      SELECT survey_id, COALESCE(max(ingested_at)::VARCHAR, '') AS m, count(*)::VARCHAR AS n
      FROM {ducklake_alias}.raw.survey_questions GROUP BY survey_id
    ),
    o AS (
      SELECT survey_id, COALESCE(max(ingested_at)::VARCHAR, '') AS m, count(*)::VARCHAR AS n
      FROM {ducklake_alias}.raw.survey_question_options GROUP BY survey_id
    ),
    r AS (
      SELECT survey_id,
             COALESCE(max(ingested_at)::VARCHAR, '') AS m,
             COALESCE(max(deleted_detected_at)::VARCHAR, '') AS d,
             count(*)::VARCHAR AS n
      FROM {ducklake_alias}.raw.responses GROUP BY survey_id
    )
    SELECT s.survey_id,
           concat_ws('|',
             s.m, s.d, s.n, s.v,
             COALESCE(q.m, ''), COALESCE(q.n, '0'),
             COALESCE(o.m, ''), COALESCE(o.n, '0'),
             COALESCE(r.m, ''), COALESCE(r.d, ''), COALESCE(r.n, '0')
           ) AS source_watermark
    FROM s
      LEFT JOIN q USING (survey_id)
      LEFT JOIN o USING (survey_id)
      LEFT JOIN r USING (survey_id)"))
}

pub_build_state <- function(con) {
  DBI::dbGetQuery(con, glue::glue("SELECT * FROM {ducklake_alias}.meta.pub_state"))
}

# NA-safe "these differ". NA on either side counts as a difference: an unknown
# value can never be evidence that a rebuild is unnecessary, so it falls to the
# safe side (a redundant rebuild) rather than the unsafe one (a skipped change).
differs <- function(x, y) is.na(x) | is.na(y) | x != y

# Pure decision function, easy to unit test without a database -- deliberately
# the same shape as decide_refresh() (ingest.R), and with the same fail-safe
# direction: getting an answer wrong here must cost a redundant rebuild, never
# a stale `pub` row. `pub` is rebuildable from `raw` alone (ADR-008), so a
# redundant rebuild is only ever wasted time.
#
# `build` describes how this run would build a survey. It is compared as well
# as the watermark because identical input still produces different output
# under a different setting: `language` and `tz` change the values written, and
# `wide_views` changes whether the view exists at all. A change to the pub SQL
# itself is handled a level up, by the schema minor version (ADR-018).
decide_rebuild <- function(survey_ids, watermarks, prior, build) {
  current <- watermarks$source_watermark[match(survey_ids, watermarks$survey_id)]
  i <- match(survey_ids, prior$survey_id)
  reason <- rep(NA_character_, length(survey_ids))

  unset <- function() is.na(reason)
  reason[unset() & is.na(i)] <- "never built"
  # A survey_id with no watermark row is one that isn't in raw.surveys at all
  # (pub_layer(surveys = ) accepts any id the caller passes). There is nothing
  # to compare, so it can never be proven unchanged.
  reason[unset() & is.na(current)] <- "no rows in raw for this survey"
  reason[unset() & differs(prior$source_watermark[i], current)] <- "raw data changed"
  reason[unset() & differs(prior$language[i], build$language)] <- "language changed"
  reason[unset() & differs(prior$tz[i], build$tz)] <- "tz changed"
  reason[unset() & differs(prior$wide_views[i], build$wide_views)] <- "wide_views changed"

  tibble::tibble(
    survey_id = survey_ids,
    rebuild = !is.na(reason),
    reason = ifelse(is.na(reason), "unchanged", reason)
  )
}

# What this run was built from and with, committed alongside the rows it
# describes (see rebuild_survey_atomically()).
record_pub_state <- function(con, survey_id, watermark, build, now) {
  DBI::dbExecute(con, glue::glue(
    "DELETE FROM {ducklake_alias}.meta.pub_state
     WHERE survey_id = {DBI::dbQuoteString(con, survey_id)}"
  ))
  write_rows_generic(con, "meta.pub_state", tibble::tibble(
    survey_id = survey_id,
    source_watermark = as.character(watermark),
    language = build$language,
    tz = build$tz,
    wide_views = build$wide_views,
    built_at = now
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
                                      watermark, build, now = Sys.time()) {
  committed <- FALSE
  DBI::dbExecute(con, "BEGIN")
  on.exit(if (!committed) try(DBI::dbExecute(con, "ROLLBACK"), silent = TRUE), add = TRUE)

  rebuild_pub_survey(con, survey_id, language, tz)
  if (wide_views) {
    rebuild_wide_view(con, survey_id, title)
  }
  record_pub_state(con, survey_id, watermark, build, now)

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

  build <- list(language = language, tz = tz, wide_views = isTRUE(wide_views))

  # Read once, before any rebuild transaction opens, and recorded verbatim for
  # every survey built in this run. Reading it *first* -- rather than after
  # each rebuild -- is what makes a concurrent ingest() safe: a rebuild's own
  # transaction can only ever see a snapshot at or newer than this one, so the
  # worst case is recording a watermark slightly older than the data actually
  # built from, which costs one redundant rebuild on the next run. The reverse,
  # recording a watermark newer than the data built and so skipping a real
  # change forever, cannot happen in this order.
  watermarks <- pub_source_watermarks(con)
  decisions <- if (isTRUE(force)) {
    tibble::tibble(survey_id = survey_ids, rebuild = TRUE, reason = "force = TRUE")
  } else if (!is.null(surveys)) {
    # An explicit surveys= is a request to rebuild those surveys, exactly as
    # ingest(surveys = ) is a request to refresh them: the caller has said what
    # they want and change detection does not get to overrule it.
    tibble::tibble(survey_id = survey_ids, rebuild = TRUE, reason = "explicit surveys= argument")
  } else {
    decide_rebuild(survey_ids, watermarks, pub_build_state(con), build)
  }
  to_rebuild <- decisions$survey_id[decisions$rebuild]

  # pub.surveys is rebuilt unconditionally, for every survey considered rather
  # than only the changed ones. It is one statement over one small row per
  # survey (*measured*: 0.17s for 50 surveys, against 25s+ for their per-survey
  # rebuilds), and keeping it outside the incremental path means survey-level
  # facts -- a retitling, a status change, an is_deleted flag -- can never be
  # the thing that goes stale while the expensive tables are skipped.
  DBI::dbExecute(con, "BEGIN")
  rebuild_pub_surveys(con, survey_ids, tz)
  DBI::dbExecute(con, "COMMIT")

  if (length(to_rebuild) == 0) {
    return(invisible(character(0)))
  }

  titles <- DBI::dbGetQuery(con, glue::glue(
    "SELECT survey_id, title FROM {ducklake_alias}.pub.surveys
     WHERE survey_id IN ({id_list_sql(con, to_rebuild)})"
  ))
  now <- Sys.time()

  for (survey_id in to_rebuild) {
    # or_default(), not %||%: a survey with no title, or an id that isn't
    # in raw.surveys at all, gives NA here rather than NULL, which %||%
    # does not catch -- and slugify(NA) yields the placeholder "x", so the
    # view came out named `wide_x_<id>` instead of falling back to the id.
    title <- or_default(titles$title[titles$survey_id == survey_id][1], survey_id)
    watermark <- watermarks$source_watermark[match(survey_id, watermarks$survey_id)]
    rebuild_survey_atomically(
      con, survey_id, title, language, tz, wide_views, watermark, build, now
    )
  }

  invisible(to_rebuild)
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
