# Idempotent DDL for the raw/meta/pub schemas. DuckLake supports no primary
# key, unique, foreign key, or check constraints (ADR-006) -- uniqueness and
# referential integrity are asserted after the fact by db_check(), not
# declared here. Every `raw` column is VARCHAR or JSON (ADR-003); typing
# happens only in `pub`, built separately by pub_layer().

# The application database's schema version, major.minor (ADR-018). The two
# halves mean different things because the two layers are worth different
# amounts: `raw` is the archive and cannot be regenerated once the API has
# moved on, while `pub` is derived and can always be thrown away and rebuilt.
#
# major -- the archival layer. Bump when `raw`'s layout or the meaning of its
#   columns changes, or for any other change an existing database cannot simply
#   be carried across. ingest() and pub_layer() both refuse to run against a
#   database stamped with a different major: it must be archived and rebuilt
#   from the API, or migrated by hand and restamped. There is deliberately no
#   automatic migration -- guessing at how to reshape an archive is exactly the
#   kind of judgement call ADR-003 keeps out of the archival path.
# minor -- the publication layer. Bump when `pub`'s tables, or the SQL that
#   fills them, change. Nothing at risk, so nothing to throw away: pub_layer()
#   drops and recreates the `pub` schema and rebuilds every survey once. This
#   is what stops `pub` drifting -- a table created by an older version keeps
#   its old columns forever otherwise, since the DDL is CREATE IF NOT EXISTS.
#
# A database stamped with the pre-split single `version` N reads as major 1,
# minor N: every version so far has been a publication-layer change.
schema_major <- 1L
schema_minor <- 3L

raw_tables <- list(
  surveys = "
    survey_id VARCHAR, title VARCHAR, type VARCHAR, status VARCHAR,
    created_on VARCHAR, modified_on VARCHAR, team VARCHAR,
    statistics JSON, links JSON, payload JSON,
    ingested_at TIMESTAMP, run_id VARCHAR,
    is_deleted BOOLEAN, deleted_detected_at TIMESTAMP",
  survey_definitions = "
    survey_id VARCHAR, payload JSON,
    ingested_at TIMESTAMP, run_id VARCHAR",
  survey_pages = "
    survey_id VARCHAR, page_id VARCHAR, title JSON, description VARCHAR,
    properties JSON, page_order INTEGER, payload JSON,
    ingested_at TIMESTAMP, run_id VARCHAR",
  survey_questions = "
    survey_id VARCHAR, question_id VARCHAR, page_id VARCHAR,
    base_type VARCHAR, type VARCHAR, title JSON, shortname VARCHAR,
    varname VARCHAR, description VARCHAR, properties JSON,
    show_rules_ids JSON, question_order INTEGER, payload JSON,
    ingested_at TIMESTAMP, run_id VARCHAR",
  survey_question_options = "
    survey_id VARCHAR, question_id VARCHAR, option_id VARCHAR, title JSON,
    value VARCHAR, properties JSON, option_order INTEGER, payload JSON,
    ingested_at TIMESTAMP, run_id VARCHAR",
  responses = "
    survey_id VARCHAR, response_id VARCHAR, status VARCHAR,
    is_test_data VARCHAR, date_submitted VARCHAR, date_started VARCHAR,
    date_updated VARCHAR, session_id VARCHAR, language VARCHAR,
    link_id VARCHAR, contact_id VARCHAR, response_time VARCHAR,
    url_variables JSON, data_quality JSON, survey_data JSON, payload JSON,
    ingested_at TIMESTAMP, run_id VARCHAR,
    is_deleted BOOLEAN, deleted_detected_at TIMESTAMP",
  survey_statistics = "
    survey_id VARCHAR, question_id VARCHAR, type VARCHAR, stats JSON,
    ingested_at TIMESTAMP, run_id VARCHAR",
  survey_campaigns = "
    survey_id VARCHAR, campaign_id VARCHAR, payload JSON,
    ingested_at TIMESTAMP, run_id VARCHAR"
  # No `contacts`/`contact_lists` here. They were declared before anything
  # fetched them, so every database shipped two permanently empty tables that
  # looked like a feature. Contacts carry heavy PII (ADR-009) and would need an
  # explicit opt-in; declare the tables in the same change that populates them.
)

meta_tables <- list(
  runs = "
    run_id VARCHAR, started_at TIMESTAMP, finished_at TIMESTAMP,
    status VARCHAR, package_version VARCHAR, duckdb_version VARCHAR,
    n_checked INTEGER, n_refreshed INTEGER, n_failed INTEGER, n_requests INTEGER",
  run_events = "
    run_id VARCHAR, survey_id VARCHAR, phase VARCHAR, status VARCHAR,
    http_status INTEGER, message VARCHAR,
    started_at TIMESTAMP, finished_at TIMESTAMP, n_responses INTEGER",
  survey_state = "
    survey_id VARCHAR, last_modified_on VARCHAR,
    last_probe_total_count INTEGER, last_probe_max_date_updated VARCHAR,
    last_refresh_started_at TIMESTAMP, last_successful_refresh_at TIMESTAMP,
    consecutive_failures INTEGER",
  integrity_checks = "
    run_id VARCHAR, survey_id VARCHAR, check_name VARCHAR,
    passed BOOLEAN, message VARCHAR, checked_at TIMESTAMP",
  # One row per load_pub_layer() run. This is the only record of the Load
  # stage anywhere the pipeline can read it: the destination service account
  # is write-only by design, so `tables` (a JSON array of the destination
  # table names written) is also what lets the next load drop tables it
  # previously wrote and no longer produces -- e.g. a wide view whose survey
  # was retitled -- without ever guessing at names it doesn't own.
  loads = "
    load_id VARCHAR, started_at TIMESTAMP, finished_at TIMESTAMP,
    status VARCHAR, destination VARCHAR, n_tables INTEGER, n_rows INTEGER,
    tables JSON, message VARCHAR",
  # One row per survey built into `pub`, recording what it was built *from* and
  # *with* (ADR-017). pub_layer() rebuilds a survey only when one of these no
  # longer matches, so a run where nothing changed upstream does no work.
  # Every column is part of that comparison: `source_watermark` covers the raw
  # rows, and `language`/`tz`/`wide_views` cover the build, since each of them
  # produces different output from identical input.
  #
  # A change to the pub SQL itself is *not* tracked here. That is what the
  # schema minor version is for (ADR-018): it invalidates the whole layer at
  # once, table shapes included, which a per-survey column could never do.
  pub_state = "
    survey_id VARCHAR, source_watermark VARCHAR, language VARCHAR, tz VARCHAR,
    wide_views BOOLEAN, built_at TIMESTAMP",
  schema_version = "major INTEGER, minor INTEGER"
)

# Typed, language-resolved (ADR-008); built by pub_layer(), never by
# ingest(). Unlike raw/meta, this schema has no fixed table set the way
# raw/meta do beyond these five -- pub_layer() only ever writes here.
pub_tables <- list(
  surveys = "
    survey_id VARCHAR, title VARCHAR, type VARCHAR, status VARCHAR,
    created_on TIMESTAMP, modified_on TIMESTAMP, team VARCHAR, is_deleted BOOLEAN",
  questions = "
    survey_id VARCHAR, question_id VARCHAR, page_id VARCHAR, type VARCHAR,
    title VARCHAR, shortname VARCHAR, varname VARCHAR, question_order INTEGER",
  options = "
    survey_id VARCHAR, question_id VARCHAR, option_id VARCHAR, title VARCHAR,
    value VARCHAR, option_order INTEGER",
  responses = "
    survey_id VARCHAR, response_id VARCHAR, status VARCHAR, is_test_data BOOLEAN,
    date_submitted TIMESTAMP, date_started TIMESTAMP, date_updated TIMESTAMP,
    session_id VARCHAR, language VARCHAR, link_id VARCHAR, contact_id VARCHAR,
    response_time INTEGER, is_deleted BOOLEAN",
  answers = "
    survey_id VARCHAR, response_id VARCHAR, question_id VARCHAR,
    question_shortname VARCHAR, question_title VARCHAR, option_id VARCHAR,
    answer VARCHAR, reporting_value VARCHAR, shown BOOLEAN, is_deleted BOOLEAN"
)

# Exact column order for each raw.*/meta.* table, parsed once from the DDL
# above. ingest() uses this to reorder/validate a tibble before writing it,
# since the INSERT ... SELECT * FROM <registered data.frame> pattern maps
# columns by *position*, not name -- a tibble built in a different column
# order would silently write values into the wrong columns instead of
# failing loudly (or would fail loudly with a confusing type-mismatch error,
# which is how this helper earned its comment).
table_columns <- function(ddl) {
  unname(vapply(
    strsplit(ddl, ",")[[1]],
    function(col) strsplit(trimws(col), "\\s+")[[1]][1],
    character(1)
  ))
}
raw_table_columns <- lapply(raw_tables, table_columns)
meta_table_columns <- lapply(meta_tables, table_columns)

# The natural key of each raw table, used to match a fetched row against the
# stored one (ADR-005). DuckLake declares no primary keys, so these are
# asserted after the fact by db_check() rather than enforced here.
raw_table_keys <- list(
  surveys = "survey_id",
  survey_definitions = "survey_id",
  survey_pages = c("survey_id", "page_id"),
  survey_questions = c("survey_id", "question_id"),
  survey_question_options = c("survey_id", "question_id", "option_id"),
  responses = c("survey_id", "response_id"),
  survey_statistics = c("survey_id", "question_id"),
  survey_campaigns = c("survey_id", "campaign_id")
)

# Columns describing *when we stored* a row rather than what Alchemer said, so
# they are excluded when deciding whether a fetched row differs from the stored
# one. Including them would make every row differ on every run -- `ingested_at`
# is stamped fresh each time -- which is exactly the write amplification the
# merge exists to avoid.
bookkeeping_columns <- c("ingested_at", "run_id")
# pub_layer.R's writes are hand-written INSERT ... BY NAME SELECT statements,
# not a registered-data.frame INSERT ... SELECT * -- BY NAME already matches
# by column name (and fails loudly on a mismatch) with no positional-order
# risk to guard against, so there is no pub_table_columns here to parallel
# raw_table_columns/meta_table_columns above.

# For read-only callers, which can't run ensure_schema(): a database last
# opened by an older version of the package is missing any table added since,
# and a read-only connection has no way to add it. Reading such a table has to
# degrade to "no rows", not error.
table_exists <- function(con, schema, table) {
  DBI::dbGetQuery(con, glue::glue(
    "SELECT COUNT(*) AS n FROM information_schema.tables
     WHERE table_catalog = {DBI::dbQuoteString(con, ducklake_alias)}
       AND table_schema = {DBI::dbQuoteString(con, schema)}
       AND table_name = {DBI::dbQuoteString(con, table)}"
  ))$n > 0
}

create_tables <- function(con, schema, tables) {
  DBI::dbExecute(con, glue::glue("CREATE SCHEMA IF NOT EXISTS {ducklake_alias}.{schema}"))
  for (name in names(tables)) {
    DBI::dbExecute(con, glue::glue(
      "CREATE TABLE IF NOT EXISTS {ducklake_alias}.{schema}.{name} ({tables[[name]]})"
    ))
  }
}

# Runs on every non-read-only alchemer_db() call. Safe to re-run: every
# statement is IF NOT EXISTS, so a normal connection never issues DDL after
# the first time it creates a fresh database.
ensure_schema <- function(con) {
  create_tables(con, "raw", raw_tables)
  create_tables(con, "meta", meta_tables)
  create_tables(con, "pub", pub_tables)

  split_schema_version_table(con)
  if (is.null(db_schema_version(con))) {
    stamp_schema_version(con, schema_major, schema_minor)
  }
  # Nothing else is stamped here, and in particular nothing is *re*-stamped: a
  # version that no longer matches the code is a fact the callers need to see.
  # Acting on it belongs to them -- pub_layer() rebuilds on a minor difference,
  # and both writers refuse to run on a major one (assert_schema_compatible()).
  invisible(TRUE)
}

# meta.schema_version held a single `version INTEGER` before the split into
# major/minor. The table holds one row of one number, so it is recreated in the
# new shape rather than ALTERed -- less machinery, and the only thing worth
# preserving (the number) is read first. Every version stamped under the old
# scheme was a publication-layer change, so N becomes major 1, minor N.
split_schema_version_table <- function(con) {
  columns <- DBI::dbGetQuery(con, glue::glue(
    "SELECT column_name FROM information_schema.columns
     WHERE table_catalog = {DBI::dbQuoteString(con, ducklake_alias)}
       AND table_schema = 'meta' AND table_name = 'schema_version'"
  ))$column_name
  if (!("version" %in% columns) || "major" %in% columns) {
    return(invisible(FALSE))
  }

  old <- DBI::dbGetQuery(
    con, glue::glue("SELECT version FROM {ducklake_alias}.meta.schema_version")
  )$version
  DBI::dbExecute(con, glue::glue("DROP TABLE {ducklake_alias}.meta.schema_version"))
  DBI::dbExecute(con, glue::glue(
    "CREATE TABLE {ducklake_alias}.meta.schema_version ({meta_tables$schema_version})"
  ))
  if (length(old) > 0) {
    stamp_schema_version(con, 1L, as.integer(old[1]))
  }
  invisible(TRUE)
}

# NULL for a database that has never been stamped, which is not an error: a
# read-only caller can meet one, and so can a database created before the
# version table was populated.
db_schema_version <- function(con) {
  stamped <- DBI::dbGetQuery(
    con, glue::glue("SELECT major, minor FROM {ducklake_alias}.meta.schema_version")
  )
  if (nrow(stamped) == 0) {
    return(NULL)
  }
  list(major = as.integer(stamped$major[1]), minor = as.integer(stamped$minor[1]))
}

stamp_schema_version <- function(con, major, minor) {
  DBI::dbExecute(con, glue::glue("DELETE FROM {ducklake_alias}.meta.schema_version"))
  DBI::dbExecute(con, glue::glue(
    "INSERT INTO {ducklake_alias}.meta.schema_version VALUES ({as.integer(major)}, {as.integer(minor)})"
  ))
  invisible(TRUE)
}

# Called by the two functions that write data -- ingest() and pub_layer() --
# before they do anything else (ADR-018). Deliberately *not* called from
# alchemer_db(), so db_status(), db_check(), and a plain read-only connection
# all still work on a database this refuses to write to: being told to archive
# and rebuild is precisely when someone needs to look inside it first.
#
# Any difference in the major version fails, in both directions. A database
# older than the code may be missing structure the code assumes; one newer than
# the code may have been written with meanings this version doesn't know. Both
# are unsafe to write to, and neither can be guessed at automatically.
assert_schema_compatible <- function(con, db) {
  stamped <- db_schema_version(con)
  if (is.null(stamped) || identical(stamped$major, schema_major)) {
    return(invisible(TRUE))
  }
  cli::cli_abort(
    c(
      "The application database at {.path {db}} is schema version
       {stamped$major}.{stamped$minor}; this version of alchemeR writes
       {schema_major}.{schema_minor}.",
      "x" = "The major version differs, which means the archival ({.code raw}) layer's
             layout or meaning is not the one this code expects. There is no automatic
             migration -- reshaping an archive on a guess is exactly what {.code raw}
             exists to prevent.",
      "i" = "Archive this directory, point {.envvar ALCHEMER_DB} at a fresh one, and
             rebuild it with {.run alchemeR::ingest()}.",
      "i" = "Or migrate it by hand and restamp {.code meta.schema_version}. Only
             {.code raw} and {.code meta} hold anything unrecoverable; {.code pub} is
             rebuilt from {.code raw} either way."
    ),
    class = "alchemeR_schema_version_error", call = NULL
  )
}

# Drops everything in `pub` and recreates it from the current DDL. Safe in a way
# no other schema reset in this package would be, and for one specific reason:
# `pub` is derived from `raw` alone (ADR-008), so the rows discarded here are
# exactly reproducible. Views go first: they are only ever wide_* pivots over
# the tables about to be dropped, and pub_layer() regenerates them per survey.
#
# Everything found in the schema is dropped, not just the tables named in
# pub_tables, so a table left behind by an older version -- the drift this
# exists to remove -- goes too.
reset_pub_schema <- function(con) {
  objects <- DBI::dbGetQuery(con, glue::glue(
    "SELECT table_name, table_type FROM information_schema.tables
     WHERE table_catalog = {DBI::dbQuoteString(con, ducklake_alias)}
       AND table_schema = 'pub'"
  ))
  views <- objects$table_name[objects$table_type == "VIEW"]
  tables <- objects$table_name[objects$table_type != "VIEW"]

  for (view in views) {
    DBI::dbExecute(con, glue::glue(
      "DROP VIEW IF EXISTS {ducklake_alias}.pub.{DBI::dbQuoteIdentifier(con, view)}"
    ))
  }
  for (table in tables) {
    DBI::dbExecute(con, glue::glue(
      "DROP TABLE IF EXISTS {ducklake_alias}.pub.{DBI::dbQuoteIdentifier(con, table)}"
    ))
  }
  create_tables(con, "pub", pub_tables)

  # meta.pub_state goes with them. It lives in `meta` but it belongs to the
  # publication layer -- it is a record of what was built, not archival data --
  # so it is dropped and recreated rather than emptied: its own columns are as
  # capable of drifting as pub's are, and every row in it described rows that
  # no longer exist.
  DBI::dbExecute(con, glue::glue("DROP TABLE IF EXISTS {ducklake_alias}.meta.pub_state"))
  DBI::dbExecute(con, glue::glue(
    "CREATE TABLE {ducklake_alias}.meta.pub_state ({meta_tables$pub_state})"
  ))
  invisible(TRUE)
}

# A short unique string for scoping a temp table/view name to one call, so
# concurrent writes on the same connection (there are none today, but the
# pattern is shared by db_schema.R and ingest.R) can't collide.
#
# Uses tempfile() rather than sample(letters): sample() draws from the user's
# RNG stream, so every ingest() silently changed the random numbers a script
# would get afterwards -- a surprising side effect from a function that only
# needs a name nobody else is using.
random_suffix <- function(n = 12) {
  substr(gsub("^file", "", basename(tempfile(pattern = "file"))), 1, n)
}

# Appends `rows` to raw.<table>, reordered/validated against the table's
# declared column order (raw_table_columns) -- required because
# INSERT ... SELECT * FROM <registered data.frame> maps columns positionally,
# not by name, so a tibble built in the wrong order would silently write
# values into the wrong columns instead of failing.
write_rows <- function(con, table, rows) {
  if (nrow(rows) == 0) {
    return(invisible(NULL))
  }
  rows <- as.data.frame(dplyr::select(rows, dplyr::all_of(raw_table_columns[[table]])))
  tmp_name <- paste0("tmp_write_", table, "_", random_suffix())
  duckdb::duckdb_register(con, tmp_name, rows)
  DBI::dbExecute(con, glue::glue("INSERT INTO {ducklake_alias}.raw.{table} SELECT * FROM {tmp_name}"))
  duckdb::duckdb_unregister(con, tmp_name)
  invisible(NULL)
}

# Reconcile a fetched set of rows against what is stored, writing only what
# actually changed.
#
# Every refresh still downloads *everything* for the survey (ADR-004 is
# untouched -- this is about what gets written, never about what gets asked
# for). What changed is that the stored rows are no longer deleted and
# rewritten wholesale. DuckLake never modifies a Parquet file in place, so a
# delete-then-insert of a whole survey writes a fresh copy of every row on
# every run: a 5,000-response survey polled every 15 minutes cost ~2 MB per
# refresh, ~200 MB/day, all of it retained until snapshots expire. Measured
# over 12 refreshes of such a survey: 2.1 MB -> 26.7 MB replacing, versus
# 2.1 MB -> 2.1 MB merging.
#
# `on_vanished` decides what happens to rows that are stored but absent from
# the fetch -- the set difference that is the only way to detect an upstream
# deletion (ADR-005):
#   "flag"   is_deleted = TRUE, never removed (responses, surveys)
#   "delete" removed, matching the previous replace-wholesale behaviour for
#            definition/statistics/campaign rows, which are derived metadata
#            rather than research data
#
# A row that reappears upstream is un-flagged automatically, because
# `is_deleted` is one of the compared columns and the fetched row always
# carries FALSE.
merge_survey_rows <- function(con, table, survey_id, rows, on_vanished = c("delete", "flag"),
                              now = Sys.time()) {
  on_vanished <- match.arg(on_vanished)
  columns <- raw_table_columns[[table]]
  keys <- raw_table_keys[[table]]
  scope <- if (is.null(survey_id)) {
    ""
  } else {
    glue::glue("survey_id = {DBI::dbQuoteString(con, survey_id)} AND")
  }

  # Nothing fetched -- which is a real state, not an error: a survey whose
  # responses were all deleted, or an account whose survey list came back
  # empty. Every stored row in scope has vanished, and saying so directly
  # avoids registering an empty frame just to take a set difference against
  # it. `ncol(rows) == 0` covers a parse that produced no columns at all
  # (an empty bind_rows()), which is the same case arriving by another route.
  if (ncol(rows) == 0 || nrow(rows) == 0) {
    flag_or_delete_vanished(con, table, on_vanished, scope, absent = "TRUE", now = now)
    return(invisible(NULL))
  }

  rows <- as.data.frame(dplyr::select(rows, dplyr::all_of(columns)))
  batch <- paste0("tmp_merge_", table, "_", random_suffix())
  duckdb::duckdb_register(con, batch, rows)
  on.exit(duckdb::duckdb_unregister(con, batch), add = TRUE)

  key_match <- paste(glue::glue("t.{keys} = s.{keys}"), collapse = " AND ")
  compared <- setdiff(columns, c(keys, bookkeeping_columns))
  changed <- paste(glue::glue("t.{compared} IS DISTINCT FROM s.{compared}"), collapse = " OR ")

  DBI::dbExecute(con, glue::glue(
    "MERGE INTO {ducklake_alias}.raw.{table} t USING {batch} s ON ({key_match})
     WHEN MATCHED AND ({changed}) THEN UPDATE
     WHEN NOT MATCHED THEN INSERT"
  ))

  key_tuple <- paste(keys, collapse = ", ")
  absent <- glue::glue(
    "({key_tuple}) NOT IN (SELECT {key_tuple} FROM {batch})"
  )
  flag_or_delete_vanished(con, table, on_vanished, scope, absent, now)
  invisible(NULL)
}

flag_or_delete_vanished <- function(con, table, on_vanished, scope, absent, now) {
  if (identical(on_vanished, "flag")) {
    DBI::dbExecute(con, glue::glue(
      "UPDATE {ducklake_alias}.raw.{table}
       SET is_deleted = TRUE, deleted_detected_at = {DBI::dbQuoteLiteral(con, now)}
       WHERE {scope} (is_deleted IS NULL OR NOT is_deleted) AND {absent}"
    ))
  } else {
    DBI::dbExecute(con, glue::glue(
      "DELETE FROM {ducklake_alias}.raw.{table} WHERE {scope} {absent}"
    ))
  }
  invisible(NULL)
}
