# alchemeR 0.1.0

## Breaking changes

* `fetch_survey()` now returns a labelled tibble and keeps `survey_<id>.rds` in
  `dir` (default `"data_raw"`, relative to the working directory, created if missing)
  instead of writing a CSV. It updates the file incrementally and makes no
  request at all within `min_age` seconds of the last check. The `survey_name`
  argument is gone; new arguments are `dir`, `refresh`, `min_age`,
  `full_refresh_days` and `include_test` (test data is excluded by default).
* Columns are named from the survey definition (`Q<id>`, `Q<id>_<option id>`
  for checkbox, rank and textbox-list items, `Q<column id>_r<row>` for custom
  tables (matrix), `url_<name>` for single sign-on surveys; a URL-shaped
  name such as `.../claims/givenname` becomes `url_givenname` unless two clash), in
  questionnaire order and present even when nobody answered. Question text is
  the column's `label` attribute. Sliders, ranks and quiz scores are numbers; dates are Toronto-time date-times; `is_test_data` is
  logical. Choice questions hold the survey's reporting values.
* `fetch_data_dictionary()` reads the survey definition (one request) and
  returns one row per option with new columns: `qid`, `parent`, `type`,
  `base_type`, `r_type`, `numeric_text`, `label`, `option_id`, `option_label`, `option_value`,
  `row_id` (custom-table row),
  `page` and `order`.
* `all_surveys()` returns tidy columns (`id`, `title`, `status`, `type`, `team`,
  `created_on`, `modified_on`).
* Credentials default to the environment variables `ALCHEMER_API_TOKEN` and
  `ALCHEMER_API_SECRET`.

## New

* `inst/quarto/Archive_account.qmd` keeps an RDS file for every survey of an
  account and reports, per survey, the status (ok, warning, error), the warnings
  and errors, the run message, the newest response update, the request count
  and the run settings. Each run appends its rows to `archive_log.csv`.
* One message per `fetch_survey()` call says what it did, why, and how many
  requests it needed.
* `fetch_survey()` creates `dir` if it does not exist.
* A warning is raised when answers in the raw data could not be placed in a
  column, so a survey with an unknown answer shape cannot lose data silently.

## Bug fixes

* `all_surveys()` returned at most 250 surveys, `fetch_data_dictionary()` at
  most 25 questions and `fetch_survey()` at most 300 pages without a warning.
  All pages are now read and checked against the total reported by Alchemer.
* A failed or partial download no longer overwrites a good file.
* A blank response no longer removes the answers of the whole page it was on.
* Requests are throttled and retried on rate limits and server errors; the API
  token never appears in an error message.
* All API calls are read-only (GET).
* Text boxes set to numbers stay text (Alchemer does not enforce the setting,
  and they often hold ids with leading zeros). The text is cleaned (spaces,
  `1,000`, student numbers `123-456-789` → `123456789`, `n/a` → `NA`), and a
  value that is still not a number raises a warning but is kept. The dictionary
  has a new column `numeric_text` marking them.
* Large surveys download about 4 times faster: pages of 250 rows, up to six
  requested at once after the first (still at most 2 requests per second).
* A response submitted or deleted while a multi-page download runs no longer
  makes the download fail: the pull is restarted once, and it is an error
  only if the count differs again.
