# alchemeR 1.0.0

alchemeR is now two things: an archive of your Alchemer account, and a pipeline
that keeps it — and optionally your analytics database — current.

## New

* `ingest()` downloads the whole account into a local DuckLake application
  database (`raw`/`pub`/`meta` schemas), so analysis no longer depends on
  continued Alchemer API access. Later runs refresh only surveys that actually
  changed, and write only the rows that differ. See `vignette("data-model")`
  and `vignette("getting-started")`.
* The database can be read while the pipeline writes to it, so an analyst's
  session never blocks a scheduled run and is never blocked by one.
* The application database carries a `major.minor` schema version. A minor bump
  (a change to `pub` or the SQL that builds it) makes `pub_layer()` drop and
  rebuild the whole `pub` schema automatically, so it can't drift. A major bump
  (a change to the archival `raw` layer) makes `ingest()` and `pub_layer()`
  stop and tell you to archive and rebuild the directory, or migrate it by
  hand — there is no automatic migration of an archive. `db_status()` and
  `db_check()` keep working either way.
* `pub_layer()` rebuilds a survey only when the `raw` rows it is built from have
  changed, or when the `language`/`tz`/`wide_views` setting or the alchemeR
  version has. A run where nothing changed upstream now does no per-survey work
  at all; each survey that is rebuilt is rebuilt whole, in one transaction.
  `pub_layer(force = TRUE)` rebuilds regardless, and `meta.pub_state` records
  what each survey was last built from. See `vignette("data-model")`.
* `pub_layer()` builds tidy, typed, language-resolved tables from `raw`.
  `survey_wide()` computes the one-row-per-respondent shape on demand for any
  survey. For everything else, query the database directly over a plain
  `DBI::dbConnect()` connection you open yourself, using the exported
  `alchemer_db_path()` to find `ALCHEMER_DB` (see
  `vignette("getting-started")`) — there is no `alchemer_db()` connect helper
  or `alchemer_tbl()` query helper, so the ATTACH mechanics aren't hidden,
  RStudio's Connections pane can still see the connection, and reading is
  just SQL or stock `dbplyr::in_schema()` (ADR-016).
* `load_pub_layer()` and `load_pipeline_health()` complete the pipeline by
  copying the publication layer, and a monitorable status row, into an external
  analytics database over a plain `DBI` connection — SQL Server via `odbc`, or
  anything else `DBI` supports. See `inst/scripts/etl_pipeline.R` and
  `vignette("scheduling")`.
* Maintenance: `db_status()`, `db_check()`, `compact()`, `expire_history()`, and
  `expunge()` (which removes data *and* its history, for retention obligations).

## Configuration

All settings come from the environment; see
`system.file("extdata", "Renviron.example", package = "alchemeR")`.

* **`ALCHEMER_DB`** (required) — the directory holding the archive.
* **`ALCHEMER_TZ`** — timezone `pub`'s timestamps are presented in. Set it to
  your Alchemer account's timezone; `raw` is never converted. See
  `vignette("data-model")`.
* **`ALCHEMER_DOMAIN`** — the API base domain is no longer hardcoded to
  `api.alchemer-ca.com`. It defaults to `api.alchemer.com`, so accounts outside
  the Canadian region now work.
* `ALCHEMER_API_TOKEN` / `ALCHEMER_API_SECRET` are read from the environment by
  default everywhere, rather than passed as bare arguments.
* `ALCHEMER_RPM` and `ALCHEMER_FULL_SWEEP_DAYS` tune request volume and the
  maximum-staleness backstop.

## Breaking changes

* `all_surveys()`, `fetch_survey()`, and `fetch_data_dictionary()` are
  deprecated in favour of `alchemer_surveys()`, `alchemer_survey()`,
  `alchemer_responses()`, `alchemer_questions()`, `alchemer_campaigns()`, and
  `alchemer_statistics()`. The old names still work with a one-time warning per
  session and will be removed in 2.0.0 — but their **output shapes have
  changed**, so this is not a drop-in shim:
    - `fetch_survey()` no longer writes a CSV by default. Pass
      `file = "path.csv"` to opt in, but the columns differ too: one
      `survey_data` column holding each response's answers as JSON, not one
      `Q<id>` column per question.
    - `fetch_data_dictionary()` returns `alchemer_questions()`'s columns
      (`id`, `title`, `shortname`, `varname`, ...), not
      `question_id`/`label`/`options_id`/`options_value`.
    - `all_surveys()` no longer flattens nested fields such as `statistics`
      into dotted column names; they come back as list-columns.

  Code relying on any of those exact shapes needs updating at the point of use,
  not just silencing the warning.

## Other changes

* All HTTP now goes through `httr2`, with throttling, retries with exponential
  backoff, and proper pagination. Requests are no longer made in a way that
  bypassed retries and throttling and passed credentials to a URL-fetching
  function.
* Maintenance is not automatic: schedule `compact()` and `expire_history()`.
  See `vignette("scheduling")`.
* Dropped the `stringr` dependency.

---

Maintainers: the design rationale, architecture decision records, and review
history live in the *Design and decision record* article on the package website
(`vignettes/articles/design.Rmd`).
