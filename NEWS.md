# alchemeR 1.0.0

alchemeR is now two things: an archive of your Alchemer account, and a pipeline
that keeps it — and optionally your analytics database — current.

## New

* `ingest()` downloads the whole account into a local DuckLake application
  database (`raw`/`pub`/`meta` schemas), so analysis no longer depends on
  continued Alchemer API access. Later runs refresh only surveys that actually
  changed, and write only the rows that differ. See `vignette("data-model")`
  and `vignette("getting-started")`.
* The database can be read while the pipeline writes to it. The DuckLake catalog
  is SQLite (`catalog.sqlite`), which supports multiple local clients, so an
  analyst's session never blocks a scheduled run and is never blocked by one.
* `pub_layer()` builds tidy, typed, language-resolved tables from `raw`, plus a
  one-row-per-respondent view per survey. `survey_wide()` computes that shape on
  demand.
* `load_pub_layer()` and `load_pipeline_health()` complete the pipeline by
  copying the publication layer, and a monitorable status row, into an external
  analytics database over a plain `DBI` connection — SQL Server via `odbc`, or
  anything else `DBI` supports. See `inst/scripts/etl_pipeline.R` and
  `vignette("scheduling")`.
* Maintenance: `db_status()`, `db_check()`, `compact()`, `expire_history()`, and
  `expunge()` (which removes data *and* its history, for retention obligations).
* `alchemer_db()` / `alchemer_tbl()` open and query the database directly, for
  dplyr and dbplyr users.

## Configuration

All settings come from the environment; see
`system.file("extdata", "Renviron.example", package = "alchemeR")`.

* **`ALCHEMER_DB`** (required) — the directory holding the archive.
* **`ALCHEMER_TZ`** — timezone for `pub`'s timestamps; set it to your Alchemer
  account's timezone. `raw` is never converted. Alchemer's unsuffixed
  timestamps (`date_updated`, `created_on`, `modified_on`) are read as already
  being in that zone, matching Alchemer's own documented example;
  `EST`/`EDT`-suffixed fields are parsed via their real offset and rendered in
  it.
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

* All HTTP now goes through `httr2` with throttling, retries with exponential
  backoff, and iterative pagination. No code path calls
  `jsonlite::fromJSON()` on a request URL, which previously bypassed retries and
  throttling and exposed credentials to a URL-fetching function.
* Dropped the `stringr` dependency.
* Package hygiene: `.Rbuildignore`, the standard `r-lib/actions` R-CMD-check
  workflow, `_pkgdown.yml`, a `lintr` config, and `.data$` references in place
  of the `utils::globalVariables()` workaround.
