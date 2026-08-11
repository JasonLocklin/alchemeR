# Alchemer Survey Ingestion Platform Specification

Version: 3.0
Status: Approved for implementation
Alchemer API documentation: https://apihelp.alchemer.com/help/api-reference
Local API reference snapshot: `docs/alchemer-api-reference.md`
Architecture decisions, rationale, and implementation roadmap: `plan.md`

This document states *what* the system must do. `plan.md` records *how* and *why*,
including the architecture decision records referenced below as ADR-nnn. Where this
document previously proposed a design that `plan.md` supersedes, the change and its
reason are recorded in §11.

## Project Refactor Context

### CURRENT STATE

Currently, users use this package in their analysis R scripts to query the Alchemer API
and pull current data into their R session. This makes all analysis scripts dependent on
continued access to the Alchemer survey API (migrating away from Alchemer or deleting a
survey from the platform will break scripts). Data loss prevention is entirely dependent
on Alchemer. This is also slow and awkward, as users must wait for the rate-limited and
paginated data downloads whenever updating their data. The package uses a hacky
page-to-file, then load-the-data-in-the-script-process approach to assist with this.

### INTENDED STATE

- User schedules an R script with `alchemeR::ingest()`, configured via environment
  variables, and all Alchemer data available to the account is saved efficiently locally
  in an open data format (consider this an application database). Then data engineers can
  process/clean/pipeline that data into a gold level of their shared analytics database,
  OR analysts can directly query that open data format for their survey data in their
  analysis scripts and do their own data cleaning and processing.
- `alchemeR::ingest()` is safe to run, which means a first run (that downloads all data)
  is done politely and avoids API rate limiting, and subsequent runs check for surveys
  with new data and only pull data from those surveys, allowing high frequency schedules
  without exploding the API requests, bandwidth, or data storage requirements.
- `alchemeR::ingest()` stores all the user's available data in the application database
  with the highest fidelity practical. Users should never find themselves needing to use
  the API to query some information that was neglected to be stored in the application
  database, and troubleshooting any data processing should be possible within the
  application database directly because the source material is available. This is an
  ingestion system and not a modeling layer. The documented exceptions are listed in §6.
- Most users, most of the time, will simply want to pull the "current response data" from
  a given survey from the application database (like they do now with the API). The
  application database architecture must make this type of query simple and efficient.
- Existing project functions will continue to be available for users wishing to download
  data directly from Alchemer via the API. However, this is no longer the primary use
  case, and breaking changes to these functions are encouraged in order to meet the
  refactor goals cleanly and efficiently. We will increment the package major version and
  document breaking changes appropriately.
- Overall package structure and files are cleaned up and meet current best practices.

### Use Cases

Data Engineers:

- Schedule a simple script that uses alchemeR functions to ingest the data, build the
  publication layer tables, and then push the latter to "source" tables on an analytics
  database. They may use dbt after that to do more detailed transformations or cleaning,
  including survey-specific logic to produce analytics marts.

R Analysts:

- May schedule a script to maintain the application database locally on their laptop or on
  a shared fileserver, or may just consume an application database from a fileserver
  maintained by someone else.
- Write analytics scripts that query tables in the application database (typically the
  publication layer) as the first step in their data cleaning and preparation of survey
  data before analysis. Analysts are R/tidyverse users and are happy working with tidy
  data; a per-survey wide shape is also provided for convenience.
- Rarely: may use time travel to investigate data quality concerns or find the data as it
  stood at a particular time in the past.
- Also rarely: may use the other package functions directly to query the survey data via
  the API into their R session for troubleshooting, or to query the absolute most
  up-to-date survey results (e.g. monitoring response counts throughout the day during a
  big data collection).

## Application database REQUIREMENTS

- All data and metadata is preserved in the application database so that:
  1. Loss of the Alchemer account never leads to loss of analytics capacity (data loss or
     loss of critical information needed to (re)process older surveys).
  2. Analysts and data engineers never need to use the Alchemer API for their work.
     Everything is available from the application database. Documented exceptions: §6.
- Must be a long-term supported local on-disk open data format, and read/write of tables
  and compute is done via DuckDB.
- Users must be able to query survey data at a past date and retrieve the response data
  available at that date. History depth equals the configured retention window (§8).

### Chosen format (ADR-001, ADR-002)

**DuckLake specification v1.0** — Parquet data files plus a catalog held in an ordinary
DuckDB file, attached through DuckDB's `ducklake` extension:

```
$ALCHEMER_DB/catalog.ducklake     # DuckLake catalog (a DuckDB database file)
$ALCHEMER_DB/data/                # Parquet data files
```

Requirements this imposes:

- `duckdb` R package **>= 1.5.2**. Earlier versions write the pre-release 0.3 spec.
  `alchemer_db()` must assert the catalog's stamped spec version is >= 1.0 and refuse to
  write otherwise.
- The `ducklake` and `json` DuckDB extensions must be installed (one-time network access,
  or pre-staged for air-gapped installs).
- One writer at a time. While `ingest()` runs it holds an exclusive lock on the catalog
  file and other processes cannot read the database. This is accepted; `ingest()` must
  fail fast with an actionable message when the lock is already held, and must refuse to
  run concurrently with itself.

History is provided by DuckLake snapshots (`AT (TIMESTAMP => ...)`,
`AT (VERSION => ...)`, `ducklake_table_changes()`). There is deliberately **no**
separately maintained archival layer and no hand-rolled row versioning; see §11 item 1.

## Key context that impacts script and application database architecture decisions

- In a typical user's environment (small scale research departments), most of the Alchemer
  surveys are static and will have no new daily data (they may be closed, or may simply
  not be actively promoted).
- Expect the data scale to be less than 100 surveys, most with hundreds of responses, and
  a few surveys with response counts in the order of 100k responses.
- Departments may be required to delete or expire survey data on Alchemer's surveys and
  this must not cause data loss in the application database.
- Alchemer tables are NOT immutable. A response in progress will change as the
  survey-taker completes the survey. A closed window may result in a survey response
  closing days later.
- Big survey pushes may result in tens of thousands of responses a day in one survey,
  while other surveys remain steady.
- Some long-running surveys may get 1 or 2 responses a day, and must not balloon data
  storage over time. (Met by DuckLake data inlining: small commits land in the catalog and
  are flushed to Parquet on compaction, so tiny daily deltas do not create a file per run.)
- Unintended data loss or corruption is the largest concern. Where freshness and
  correctness conflict, correctness wins.
- Everything is datestamped, and the architecture permits automatically expunging research
  data past a certain age or date.
- Alchemer is aggressive at protecting their API with pagination and rate limits.
  `alchemeR::ingest()` is intended to be a scheduled job, so it can afford to be polite.
- Download efficiency is a secondary concern. Most surveys are small, and re-downloading a
  large survey only happens while it is actively collecting.

## Refactor Goals

- Reliable, reproducible ingestion. Boring infrastructure.
- Minimal, maintainable, auditable code. Use only well-established libraries and minimal
  LOC to achieve requirements, with good, succinct commenting practice.
- Automatic discovery of new surveys.
- No survey-specific logic.
- No row-level watermarking (response rows are mutable).
- Survey-level refreshes via API.
- Persistent local application database.
- The application database provides a complete archive of Alchemer data, subject to §6.

## Technology Stack

### R

All R code. Minimal LOC to achieve the end goals in a production quality, auditable, and
maintainable tool. Reliability is a requirement. Maintainability is a requirement.

Keep existing package functions available to R users who may want to pull data directly
from Alchemer in their scripts, but they must be audited and refactored where necessary to
meet this spec.

Responsibilities:

- API communication
- retries
- rate limiting
- survey discovery
- state management
- logging in the application database (monitoring is out of scope, but users may monitor
  these tables for failures themselves)

The application database is an open data format and is interacted with exclusively via
DuckDB from R. dbplyr is encouraged over embedded SQL strings where it can accomplish the
same job more readably; generated DDL and `PIVOT` statements are the expected exceptions.

Rate limiting, retries with exponential backoff, and pagination must use `httr2`'s
built-in facilities (`req_throttle()`, `req_retry()`, `req_perform_iterative()`) rather
than hand-rolled loops (ADR-011). No code may bypass `httr2` by passing a request URL to
`jsonlite::fromJSON()`, as the current `fetch_survey()` does — that path has no retries,
no throttling, and no error handling.

### Configuration for `alchemeR::ingest()`

Minimal configuration via environment variables. An example `Renviron` file is provided in
`inst/extdata/Renviron.example`. The API secret may be provided directly via argument as
well as by environment variable, so users may source it from `keyring`. Other functions
follow the same pattern, so that configuration details never end up in users' project
source code.

| Variable | Default | Purpose |
|---|---|---|
| `ALCHEMER_API_TOKEN` | — | required |
| `ALCHEMER_API_SECRET` | — | required (or passed as an argument) |
| `ALCHEMER_DOMAIN` | `api.alchemer.com` | regional base domain — **required to be configurable**; the current package hardcodes the Canadian domain, and the API docs identify wrong-domain calls as one of the most common causes of failure |
| `ALCHEMER_DB` | — | application database directory |
| `ALCHEMER_RPM` | `100` | request throttle ceiling (documented account limit is 240/min, shared with interactive users) |
| `ALCHEMER_FULL_SWEEP_DAYS` | `90` | maximum staleness before a survey is refreshed regardless of change detection |

Credentials travel as query parameters because the API requires it. Full request URLs must
therefore never be written to the log tables or to console output; log method, path, and
status only.

## Data Model for the Application Database

Three schemas in one DuckLake catalog: `raw` (archival), `meta` (operational), `pub`
(publication, built separately by `pub_layer()`).

### Fidelity rule (ADR-003)

Every column in `raw` is `VARCHAR` or `JSON`. No type coercion, no renaming, no language
selection, and no boolean parsing happens at ingest. The API's typing is genuinely
inconsistent (`is_test_data` as `"1"`/`"0"`, booleans as `"True"`/`"False"`, titles as
`{"English": "..."}` maps, `data_quality` as `[]`-or-object, the literal key
`"total responses"` with a space, `results_per_page` sometimes a string). Coercing at
ingest would let a new question type or a changed representation fail an ingest or
silently mangle data. All such judgement calls belong to `pub_layer()`, which is cheap to
re-run and safe to fix retroactively.

Each `raw` row also carries the source object **verbatim** in a `payload` JSON column
(minus the keys redacted per §6), so any field not modelled explicitly is still present
and replayable. This replaces the separately proposed `raw_api_payloads` table.

### `raw` — archival layer

Common columns: `ingested_at`, `run_id`, and where deletion is detectable
`is_deleted`, `deleted_detected_at`.

| Table | Grain | Contents |
|---|---|---|
| `raw.surveys` | survey | Discovery record: survey_id, title, type, subtype, status, created_on, modified_on, team, `statistics` JSON, `links` JSON, `payload` |
| `raw.survey_definitions` | survey | The complete `GET /v5/survey/{id}` payload, verbatim — the audit and replay artefact |
| `raw.survey_pages` | survey × page | Page definitions flattened from the definition tree, with order |
| `raw.survey_questions` | survey × question | question_id, page_id, base_type, type, `title` JSON, shortname, varname, description, `properties` JSON, `show_rules_ids` JSON, order, `payload`. Everything needed to interpret responses |
| `raw.survey_question_options` | survey × question × option | option_id, `title` JSON, value (reporting value), `properties` JSON, order. Important because many surveys rely on coded responses |
| `raw.responses` | survey × response | status, is_test_data, date_submitted, date_started, date_updated, session_id, language, link_id, contact_id, response_time, `url_variables` JSON, `data_quality` JSON, `survey_data` JSON (all answers verbatim), `payload` (PII-redacted) |
| `raw.survey_statistics` | survey × question | type plus `stats` JSON |
| `raw.survey_campaigns` | survey × campaign | Campaign/link definitions plus `payload` |
| `raw.contacts`, `raw.contact_lists` | contact | Opt-in only (`include_contacts = TRUE`) |

There is no normalized answer table in `raw`: it is derivable from `survey_data` and is
provided in `pub` instead, where it is useful (§9).

### `meta` — operational layer

| Table | Grain | Contents |
|---|---|---|
| `meta.runs` | run | run_id, start/finish, status, package and DuckDB versions, counts of surveys checked/refreshed/failed, request count |
| `meta.run_events` | run × survey × phase | The refresh log: one row per refresh attempt phase with status, HTTP status, message, timings, response count |
| `meta.survey_state` | survey | Change-detection state: last `modified_on`, last probe `total_count` and `max(date_updated)`, last_refresh_started_at, last_successful_refresh_at, consecutive_failures |
| `meta.integrity_checks` | run × check | Results of the post-write assertions (§7) |

## What Must Be Retained From Alchemer

The guiding rule is: any information available through the API that could later be useful
to an analyst should be preserved. The ingestion process favours retention over filtering.

This includes survey definitions, survey metadata, survey status, survey type, question
metadata, pages, option lists, coded values, reporting values, response statuses,
timestamps, statistics where available, and campaign metadata where exposed and accessible
to the account.

## §6 Deliberate exclusions (ADR-009)

These are knowing, documented gaps in the "never need the API again" guarantee:

- **File-upload attachment bytes are not downloaded.** The answer value and Alchemer
  filename are retained, so the response record is complete, but the files themselves are
  not fetched and the pre-signed download URLs (which expire, default 300 seconds) are not
  chased. Files uploaded by respondents are therefore lost if the Alchemer account is lost.
- **Respondent IP address, latitude, longitude, city, region, postal code, DMA, country,
  and user agent are redacted at ingest**, including inside the stored `payload` JSON.
  Redaction is driven by a single auditable constant list.
- **Contacts and contact lists are opt-in** (`include_contacts = FALSE` by default),
  because they carry heavy PII: names, email addresses, phone numbers, postal addresses.
- Campaigns and survey statistics are ingested by default (1–2 requests per survey, no
  respondent PII).

## Refresh Strategy

### Chosen strategy (ADR-004)

**Full survey-level refresh. Not response-level incremental loading.** When a survey is
selected for refresh, all of its responses are downloaded and its rows are rebuilt
wholesale.

Response-level incremental loading by `date_updated` is rejected because it cannot be
made provably safe against silent data loss: `date_updated` carries no timezone suffix and
is documented as "typically UTC, but may vary by account settings"; newly submitted data
can take up to five minutes to become visible via the API; identical GETs are cached for
60 seconds; and deleted responses are silently excluded from list results, so a delta can
never observe a deletion. Any one of these can produce a permanently missing row that
nothing downstream can reconcile. Under full refresh, every one of those failure modes
degrades only to "this survey refreshed later than ideal" and self-heals on the next run.

### Change detection selects *when* to refresh, never *which rows* to store

Change-detection values are hints. A wrong hint costs freshness — bounded by
`full_sweep_days` — and can never cost correctness. Implementation code must say so
explicitly in comments.

A survey is refreshed when **any** of the following holds:

1. it has never been successfully refreshed;
2. `modified_on` from the survey list differs from the recorded value (definition edit);
3. the probe pair `(total_count, max(date_updated))` differs from the recorded value;
4. `last_successful_refresh` is older than `full_sweep_days` (default 90 — a correctness
   backstop bounding how long an undetected change could persist, not a freshness knob);
5. the caller passed `surveys =` or `force = TRUE`.

The probe is a single request per survey:
`surveyresponse?resultsperpage=1&order_by=-date_updated`, which yields `total_count` and
the newest `date_updated`. It detects new responses, edits to existing responses (any edit
bumps `date_updated`), and deletions (`total_count` falls). For ~100 surveys that is ~100
requests per run.

### Workflow

1. Acquire the catalog lock; fail fast with an actionable message if held.
2. Open the database, assert the DuckLake spec version, apply schema migrations.
3. Discover surveys via `GET /v5/survey` (paginated); upsert `raw.surveys`; flag surveys
   that have vanished.
4. Probe each survey; decide refresh candidates by the rule above; log each decision and
   its reason.
5. For each candidate, politely download the survey definition tree
   (`GET /v5/survey/{id}` — one request for pages, questions, and options), the question
   list (for `varname`, absent from the tree), campaigns, statistics, and **all**
   responses at 500 per page.
6. In a single transaction per survey, rebuild that survey's rows in every `raw` table and
   update `meta.survey_state`.
7. Run the integrity assertions (§7); roll the survey back on failure.
8. Log the outcome in its own transaction and continue to the next survey.

`ingest()` returns, invisibly, one row per survey describing the decision, counts,
timings, and status.

## §7 Reliability Requirements

### Retries

Retry on 429, 500, 502, 503, 504 with exponential backoff, via `httr2::req_retry()`.
Avoid issuing identical GETs within 60 seconds, since the API serves them from cache.

### Transactions

Every survey refresh executes inside a transaction. On failure, the transaction rolls back
and the prior state is preserved. One survey's failure never blocks the others, and every
survey already committed stays committed.

**The run log is written outside the refresh transaction** (ADR-007). Log rows committed
inside a refresh transaction would be rolled back along with the failure they describe,
destroying exactly the records a user monitoring for failures needs.

### Integrity assertions (ADR-006)

The DuckLake format supports no primary keys, unique, foreign key, or check constraints,
so uniqueness is a property of the write pattern and must be asserted rather than assumed.
After each survey commit, `ingest()` checks and records in `meta.integrity_checks`:

- no duplicate `(survey_id, response_id)` in `raw.responses`;
- the live response count matches the count the refresh reported fetching;
- every `raw.responses.survey_id` exists in `raw.surveys`;
- every question referenced by `raw.survey_question_options` exists in
  `raw.survey_questions`.

A failed assertion rolls that survey back, logs loudly, and does not stop the run.
`db_check()` exposes the same assertions on demand.

### Missing data (ADR-005)

If surveys or responses are no longer available via the API (deleted responses, deleted
surveys), the corresponding records are marked as such in the application database and are
never automatically removed from any layer. Downstream users may use the flag to exclude
deleted responses, but that is their choice.

A refresh therefore does not blind-delete. It builds the survey's new row set as *(rows
fetched from the API)* ∪ *(rows previously stored but absent from the fetch, carried
forward with `is_deleted = TRUE` and `deleted_detected_at` set)*, and swaps that in
atomically. Once flagged, a row stays flagged unless the API returns it again.

## §8 Retention and expungement

Everything is datestamped, and research data can be expunged automatically past a given
age or date:

- `expire_history(db, older_than)` trims time-travel history and reclaims the underlying
  files. Time travel to an expired snapshot then fails cleanly rather than returning
  wrong data.
- `expunge(db, survey_id =, before_date =)` removes research data and its history.

Note the trade-off users must understand: time-travel depth and expungement are the same
knob. History only reaches as far back as retention allows.

## §9 Publication Layer

### Principle

Data lightly transformed, filtered, and simplified for export to an analytics database as
"source" data, and for direct use by R analysts. This is the only layer that does not
require archival-level fidelity to Alchemer's API data structures. It does not intend to
replace a data engineering pipeline or an analyst's own cleaning and transformation work.
It exists so that the `raw` layer can maximise archival-grade fidelity without that
constraint, while downstream users get convenient source tables.

Built by a separate function, `alchemeR::pub_layer()`, so that users archiving data only
need not compute it. It is rebuildable from `raw` alone, incremental by survey, and safe
to re-run.

Goals:

- Easy consumption by R/tidyverse users: tidy data, with labels denormalised in so
  ordinary analysis needs no metadata joins.
- Sufficient for data engineering pipelines, without administrative cruft.
- Easy to sweep every publication table for a push-to-analytics-database script: a
  dedicated `pub` schema.
- Naming that is clear to human analysts and data engineers.
- No survey-specific logic; universal processing only.
- Trustworthy without auditing the archival layer.

### Tables (ADR-008)

| Table | Grain | Notes |
|---|---|---|
| `pub.surveys` | survey | Typed, resolved title and status |
| `pub.questions` | survey × question | Typed, language-resolved titles, shortname, varname, page |
| `pub.options` | survey × question × option | Option label and reporting value |
| `pub.responses` | response | Typed timestamps and booleans; test responses flagged, not dropped |
| `pub.answers` | response × question | The tidy long form: ids plus question shortname and title, option id, answer value, reporting value, `shown`, `is_deleted` |
| `pub.wide_<title_slug>_<survey_id>` | respondent | Generated **view**, one row per respondent — the shape analysts are used to, at no storage cost. e.g. `pub.wide_q1_customer_feedback_8611799` |

Wide views require an explicit pivot column list (DuckDB will not store a dynamic `PIVOT`
in a view), so `pub_layer()` generates the list from `raw.survey_questions` and
regenerates the views on each run, keeping them in step with question changes. The slug is
a lowercased, underscore-separated, ASCII-transliterated, length-capped survey title; the
`survey_id` suffix keeps names unique and stably joinable to `pub.surveys`. Because titles
change, each run drops the survey's existing `wide_*_<survey_id>` views before recreating
them, so a rename leaves no orphan behind. The duplicate-shortname collision rule must be
documented. `survey_wide(con, survey_id)` performs the same pivot on demand for users who
have not built the views.

Language selection is a `pub_layer(language = "English")` argument; multilingual titles
remain intact in `raw`.

## §10 Package requirements

- Version 1.0.0. Breaking changes documented in `NEWS.md`.
- Existing user-facing functions remain available: `all_surveys()`, `fetch_survey()`, and
  `fetch_data_dictionary()` become deprecated shims (`lifecycle::deprecate_warn()`)
  delegating to refactored equivalents that return tibbles and have no file side effects.
  `fetch_survey()` writes a CSV only when given an explicit `file =` argument.
- Package structure, tests, docs, and CI meet current best practices: `testthat` with
  fixture-based tests, `.Rbuildignore` covering development-only files, roxygen docs,
  pkgdown, vignettes, and an `r-lib/actions` R-CMD-check workflow (the README badge
  currently points at a workflow file that does not exist).
- No credentials are available during development, so the HTTP layer is injectable and
  ingestion is tested against fixture payloads derived from
  `docs/alchemer-api-reference.md` (ADR-010).

## §11 Changes from version 2.0 of this specification

1. **Removed the separate archival layer and the `raw_api_payloads` table.** Version 2.0
   proposed an archival layer, a current-state layer, `current_responses`,
   `response_answers`, and `raw_api_payloads` — up to four copies of the same bytes.
   DuckLake versions data natively, so history is free; verbatim fidelity is kept in a
   `payload` column on each row; and the normalized answer table moved to `pub`, where it
   is actually consumed. Substantially less storage and much less of the code most likely
   to harbour silent bugs.
2. **Named the storage format** and pinned its requirements (`duckdb >= 1.5.2`, spec
   version assertion). Version 2.0 left "open data format supporting historical versions"
   unresolved; see `plan.md` §2 for why DuckLake beats Iceberg and Delta for a local,
   all-R, service-free deployment.
3. **Specified refresh candidate selection.** Version 2.0 said only "determine refresh
   candidates". That unspecified step is where silent data loss would have entered, so the
   rule is now explicit, as is the principle that hints never select rows.
4. **Moved the run log outside the refresh transaction**, which version 2.0's wording would
   have rolled back along with the failures it recorded.
5. **Added the fidelity rule** that `raw` is entirely untyped, so a new question type or
   changed representation cannot fail an ingest.
6. **Added the regional domain requirement.** The current code hardcodes the Canadian
   domain, which is broken for every other account region.
7. **Added integrity assertions**, because the chosen format has no constraints.
8. **Added concurrency handling**: a lock and an actionable error, so two overlapping
   scheduled runs cannot fail confusingly mid-write.
9. **Reduced metadata request cost**: one `GET /v5/survey/{id}` returns the entire
   pages → questions → options tree, replacing a per-page and per-question crawl.
10. **Recorded deliberate exclusions** (§6) and stated plainly where the "never need the
    API again" and "loss of Alchemer never loses analytics capacity" guarantees do not
    hold.
11. **Concretised the publication layer** into named tables plus generated wide views, and
    added `survey_wide()`.
12. **Added retention semantics** (§8), including the trade-off between time-travel depth
    and expungement.
