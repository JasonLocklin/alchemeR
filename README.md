
<!-- README.md is generated from README.Rmd. Please edit that file -->

# alchemeR

<!-- badges: start -->

[![R](https://github.com/grousell/alchemeR/actions/workflows/r.yml/badge.svg)](https://github.com/grousell/alchemeR/actions/workflows/r.yml)
<!-- badges: end -->

The goal of alchemeR is to provide tools to access Alchemer survey data
from their API and pull into an R environment. More informtion on the
Alchemer API and set up can be found here:
<https://apihelp.alchemer.com/help>

## Installation

You can install the development version of alchemeR from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("grousell/alchemeR")
```

## Credentials

Create an API token and secret key in Alchemer and put them in your
`.Renviron` (`usethis::edit_r_environ()`), then restart R:

    ALCHEMER_API_TOKEN=your-token
    ALCHEMER_API_SECRET=your-secret

All functions read these two variables by default. To use `keyring`
instead, pass `token = keyring::key_get("alchemer", "token")` and
`secret_key = keyring::key_get("alchemer", "secret")`. The package only
sends read-only (GET) requests and never prints your credentials.

## Example

``` r
library(alchemeR)

all_surveys()                                   # one row per survey

df   <- fetch_survey(123)                       # a labelled tibble; keeps ./survey_123.rds
dict <- fetch_data_dictionary(123)              # one row per answer option
```

`fetch_survey()` prints one line saying what it did, for example
`Survey 123: incremental pull (37 rows changed), 4 requests, 2.1 s`. Use
`suppressMessages()` to silence it.

### What happens when you run it again

- Within `min_age` seconds (default 600) of the last check: the file is
  read, no request is made.
- Otherwise it asks Alchemer whether anything changed (2 requests). If
  not, the file is kept; if so, only recently updated responses are
  downloaded and merged.
- Everything is downloaded again when the survey was edited, an older
  response was deleted, `include_test` changed, or the oldest rows are
  over `full_refresh_days` (default 90) days old and the survey is
  active. The message tells you why and roughly how many requests it
  needs.
- `refresh = "full"` forces a full download; `refresh = "local"` reads
  the file without any request.
- A failed download never damages the file. If the file cannot be
  written (open in another program, locked by a synced folder), you get
  a warning and the data anyway.

### The data

- Columns: `survey_id`, `id`, `status`, `is_test_data`, dates (Toronto
  time), `response_time`, `language`, `link_id`, `data_quality` (JSON
  text), then one column per answer in questionnaire order: `Q<id>`, or
  `Q<id>_<option id>` for each option of checkbox and rank items (table
  rows use their own ids), and `fetched_at`. Single sign-on surveys also
  get `url_<name>` columns; other URL variables are not kept.
- Question text is the `label` attribute of each column (shown by
  RStudio’s viewer, `gtsummary`, `sjPlot`). `bind_rows()` drops labels.
- Numeric text boxes, sliders and rank items are numbers; everything
  else is text. A value in a numeric item that is not a number (for
  example `12 kg`) becomes `NA`, with a warning that shows its shape but
  not its value.
- A choice answer holds the option’s *reporting value*, not its title.
  Recode with the dictionary:
  `dict |> dplyr::filter(question_id == "Q3") |> dplyr::select(option_value, option_label)`.
- The dictionary is also in `attr(df, "alchemer")$dictionary`, next to
  the survey metadata.
- All statuses are returned (`Complete`, `Partial`, `Disqualified`);
  filter on `status`. Test data is excluded unless
  `include_test = TRUE`; use a separate `dir` if you need both.
- Signature images are not kept (only the typed name), and identifiers
  such as IP address, session and contact ids are not kept. Hidden
  values and open-text answers are returned as collected.
- Responses deleted in Alchemer disappear from the file at the next full
  download (or when they fall in the recent window).

### Several surveys, other tools

``` r
ids <- c(123, 456)
all_data <- purrr::map(ids, fetch_survey, refresh = "local") |> dplyr::bind_rows()
arrow::write_parquet(fetch_survey(123), "survey_123.parquet")   # for Python, Power BI
```

### Archive a whole account

Copy the report and render it with Quarto:

``` r
file.copy(system.file("quarto", "Archive_account.qmd", package = "alchemeR"), ".")
# quarto render Archive_account.qmd -P archive_dir:my_archive
```
