# alchemeR 0.1.0

This release fixes potential silent data loss and makes `fetch_survey()` an
incremental downloader so it is extremely fast. It returns a typed, labelled
tibble based on Alchemer's API metadata, keeps a local cache as an RDS file and
downloads only what changed each time it is run (types, labels and time zones
survive the round trip in RDS, unlike in a CSV).

## Migrating from 0.0.0.9000

* `fetch_survey(id, token, secret_key, survey_name)` wrote a CSV. Now
  `df <- fetch_survey(id)` returns the data and keeps `data_raw/survey_<id>.rds`.
  For a CSV, use `readr::write_excel_csv(df, ...)`. For Python or Power BI, use
  `arrow::write_parquet(df, ...)`.
* Column names, types and the dictionary's columns have changed (see below).
  Scripts that select or recode columns need updating.
* The first call for each survey downloads it in full. Later calls are quick.
* To run an old script, temporarily install the last version before this one
  with `pak::pak("grousell/alchemeR@679badf")`, then go back to the current
  version with `pak::pak("grousell/alchemeR")`.

## Breaking changes

* `fetch_survey()` now returns a labelled tibble and keeps `survey_<id>.rds` in
  `dir` (default `"data_raw"`, relative to the working directory, created if
  missing) instead of writing a CSV. It updates the file incrementally and makes
  no request at all within `min_age` seconds of the last check. The
  `survey_name` argument is gone. The new arguments are `dir`, `refresh`,
  `min_age`, `full_refresh_days` and `include_test`. Test data is now excluded
  by default (it used to be included). Rows are sorted by `id`.
* Columns are named from the survey definition, in questionnaire order, and
  are present even when nobody answered:
  * `Q<id>` for most items.
  * `Q<id>_<option id>` for checkbox, rank and textbox-list items.
  * `Q<column id>_r<row>` for custom tables (matrix).
  * `url_<name>` for single sign-on surveys. A URL-shaped name such as
    `.../claims/givenname` becomes `url_givenname`, unless two such names
    clash.
* New system columns: `survey_id`, `date_started`, `date_updated`, `language`,
  `link_id`, `data_quality` (JSON text) and `fetched_at` (when the row was
  downloaded).
* Question text is the column's `label` attribute. Sliders, ranks and quiz
  scores are numbers (a value that is not a number becomes `NA`, with a
  warning). Dates are Toronto-time date-times, and `is_test_data` is
  logical. Choice questions hold the survey's reporting values.
* Text boxes set to "numbers" stay text, because Alchemer does not enforce the
  setting and they often hold ids with leading zeros. The text is cleaned:
  spaces are trimmed, `1,000` → `1000`, student numbers `123-456-789` →
  `123456789` and `n/a` → `NA`. A value that is still not a number raises a
  warning but is kept.
* `fetch_data_dictionary()` reads the survey definition (one request). It
  returns one row per option. `question_id` now matches the data's column
  names, labels have no HTML tags, and `options_id`/`options_value` are renamed
  `option_id`/`option_value`. The columns are `question_id`, `qid`, `parent`,
  `type`, `base_type`, `r_type`, `numeric_text`, `label`, `option_id`,
  `option_label`, `option_value`, `page`, `row_id` (custom-table row) and
  `order`.
* `all_surveys()` returns tidy columns: `id`, `title`, `status`, `type`,
  `team`, `created_on` and `modified_on`.

## New

* `inst/quarto/Archive_account.qmd` keeps an RDS file for every survey of an
  account. For each survey it reports the status (ok, warning or error), the
  warnings and errors, the run message, the newest response update and the
  request count. It also records the run settings. Each run appends its rows to
  `archive_log.csv`.
* Each `fetch_survey()` call prints one message saying what it did, why, and
  how many requests it needed.
* A warning is raised when answers in the raw data could not be placed in a
  column, so a survey with an unknown answer shape cannot lose data silently.
* Large surveys download about 4 times faster. Pages hold 100 rows, and after
  the first page up to six are requested at once (still at most 2 requests per
  second).

## Bug fixes

* `all_surveys()` returned at most 250 surveys, `fetch_data_dictionary()` at
  most 25 questions and `fetch_survey()` at most 300 pages, without a warning.
  All pages are now read and checked against the total reported by Alchemer.
* Dates were written as text such as `2026-11-01 05:31:00 GMT`, which
  `as.POSIXct()` reads as local time, 4–5 hours off. They are now date-times, read
  as UTC and shown in Toronto time, so `date_submitted` is the true instant.
  `date_started` and `date_updated` are new. An unexpected time zone in the API
  data is an error rather than a silent shift.
* A blank response no longer removes the answers of the whole page it was on.
* A survey with a rank question could not be saved: the ranks were a list
  column, which `write.csv()` cannot write. Each ranked option is now a number
  column.
* Signature answers were dropped. The signer's typed name is now kept (the
  image is not).
* A failed or partial download no longer overwrites a good file.
* Requests are throttled to stay under the account's rate limit, which
  colleagues share. Rate limits, server errors, timeouts and dropped
  connections are retried. The API token never appears in an error message.
* A response submitted or deleted while a multi-page download runs no longer
  makes the download fail. The pull is restarted once, and it is an error only
  if the count differs again.

## Package

* Requires R >= 4.1 and httr2 >= 1.2.0. `glue`, `stringr` and `tidyr` are no
  longer needed. `library(alchemeR)` attaches `lubridate`, the recommended way
  to work with the date columns.
* The README is rewritten for analysts: setup, what happens on a re-run, the
  data, recipes and archiving.
