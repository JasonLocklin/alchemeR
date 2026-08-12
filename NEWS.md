# alchemeR 1.0.0

## Breaking changes

* `all_surveys()`, `fetch_survey()`, and `fetch_data_dictionary()` are deprecated in
  favour of `alchemer_surveys()`, `alchemer_survey()`, `alchemer_responses()`,
  `alchemer_questions()`, `alchemer_campaigns()`, and `alchemer_statistics()`. The
  deprecated functions delegate to their replacements and emit a one-time warning
  per session (`lifecycle::deprecate_warn()`); they will be removed in 2.0.0.
* `fetch_survey()` no longer writes a CSV file by default. Pass `file = "path.csv"`
  to opt into the old file-writing behaviour.
* The Alchemer API base domain is no longer hardcoded to `api.alchemer-ca.com`.
  It now defaults to `api.alchemer.com` and is configurable via `ALCHEMER_DOMAIN`,
  so accounts outside the Canadian region now work correctly.
* New ingestion pipeline: `ingest()` downloads the full Alchemer account into a
  local DuckLake application database (`raw`/`meta`/`pub` schemas), so downstream
  analysis no longer depends on continued Alchemer API access. See
  `vignette("data-model")` and `vignette("getting-started")`.
* New `pub_layer()` builds typed, English-resolved publication tables and
  generated per-survey wide views from the `raw` layer.
* New maintenance functions: `db_status()`, `db_check()`, `compact()`,
  `expire_history()`, `expunge()`.
* New `load_pub_layer()` and `load_pipeline_health()` complete an ETL pipeline
  by copying the publication layer (and a monitorable health summary) into an
  external analytics database over a plain `DBI` connection -- SQL Server via
  `odbc`, or anything else `DBI` supports. See
  `inst/scripts/etl_pipeline.R` and `vignette("scheduling")`.
* New required configuration: `ALCHEMER_DB` (application database directory).
  `ALCHEMER_API_TOKEN` and `ALCHEMER_API_SECRET` are now read from the environment
  by default across all functions, rather than being passed as bare arguments.

## Other changes

* All HTTP requests now go through `httr2` with throttling (`req_throttle()`),
  retries with exponential backoff (`req_retry()`), and iterative pagination
  (`req_perform_iterative()`). No code path calls `jsonlite::fromJSON()` on a
  request URL, which previously bypassed retries, throttling, and error handling
  and exposed credentials to a URL-fetching function.
* Dropped the `stringr` dependency.
* Package hygiene: added `.Rbuildignore`, replaced the ad hoc GitHub Actions
  workflow with the standard `r-lib/actions` `R-CMD-check.yaml`, added a
  `_pkgdown.yml` and `.lintr` configuration, and removed the
  `utils::globalVariables()` workaround in favour of `.data$` references.
