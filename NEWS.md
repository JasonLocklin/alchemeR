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
* `ingest_failures()` names the surveys currently failing to refresh and why —
  message, status code, and any failed integrity assertion. `ingest()` warns
  when a run has failures rather than reporting them only in an invisible
  return value, and that return value now carries each failure's `message` and
  `http_status` alongside its `status`. `vignette("troubleshooting")` covers
  reading them, and the causes that recur.
* `alchemer_db()` / `alchemer_tbl()` open and query the database directly, for
  dplyr and dbplyr users.

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

## Bug fixes

* **A failed survey refresh is retried on the next run.** A failure used to
  record the change-detection values it had just failed to archive, so the next
  run compared them against themselves, decided "no change detected", and
  skipped the survey — silently, for up to `ALCHEMER_FULL_SWEEP_DAYS` (90 by
  default) or until the survey changed again upstream. A survey could therefore
  sit un-refreshed with responses waiting, showing `consecutive_failures` frozen
  at 1. Change detection now stays pointed at the last *successful* state, so a
  failed survey is retried until it succeeds.

  If you are upgrading with surveys already parked this way, refresh them once
  explicitly — `ingest(surveys = c("123", "456"))` always refreshes, regardless
  of change detection — or run `ingest(force = TRUE)` once.
* `alchemer_responses()` and the other direct-API functions honour a
  `resultsperpage` passed through `...`. It was documented but silently reset
  to 500, which made a smaller page size impossible to ask for — the one knob
  that can get a very wide survey under Alchemer's 30-second response timeout.

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
