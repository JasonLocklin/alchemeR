# alchemeR 1.0.0

## Breaking changes

* `all_surveys()`, `fetch_survey()`, and `fetch_data_dictionary()` are deprecated in
  favour of `alchemer_surveys()`, `alchemer_survey()`, `alchemer_responses()`,
  `alchemer_questions()`, `alchemer_campaigns()`, and `alchemer_statistics()`. The
  deprecated functions delegate to their replacements and emit a one-time warning
  per session (`lifecycle::deprecate_warn()`); they will be removed in 2.0.0. Their
  **output shapes have changed**, not just their names -- this is not a drop-in
  compatibility shim:
    - `fetch_survey()` no longer writes a CSV file by default. Pass `file =
      "path.csv"` to opt into file-writing, but the file's columns are also
      different: one `survey_data` column holding each response's answers as a
      JSON string, not one `Q<id>` column per question as before.
    - `fetch_data_dictionary()` now returns `alchemer_questions()`'s column set
      (`id`, `title`, `shortname`, `varname`, ...), not the old
      `question_id`/`label`/`options_id`/`options_value` shape.
    - `all_surveys()` no longer flattens nested fields (e.g. `statistics`) the
      way the old `jsonlite::fromJSON(flatten = TRUE)`-based implementation did;
      they come back as list-columns instead.
  Code relying on any of these three functions' exact previous output shape will
  need to update at the point of use, not just silence the deprecation warning.
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
* `pub` timestamps are presented in the timezone named by the new `ALCHEMER_TZ`
  variable (or `pub_layer(tz = )`), defaulting to the machine's own timezone --
  set it explicitly for scheduled runs. `raw` is never converted. Alchemer's
  unsuffixed timestamps (`date_updated`, `created_on`, `modified_on`) are read as
  already being in that zone, which is what Alchemer's own documented example
  shows; `EST`/`EDT`-suffixed fields are parsed via their real offset and
  rendered in it.
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
