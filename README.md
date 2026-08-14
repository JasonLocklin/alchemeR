
<!-- README.md is generated from README.Rmd. Please edit that file -->

# alchemeR

<!-- badges: start -->

[![R-CMD-check](https://github.com/grousell/alchemeR/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/grousell/alchemeR/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

alchemeR copies your whole [Alchemer](https://www.alchemer.com/) account
into a local database and, optionally, on into your analytics database.
Two things come out of that:

- **An archive.** Every survey, question, and response is kept locally,
  in full, so analysis stops depending on continued Alchemer access —
  and so you still have the data if a survey (or the account) goes away.
- **A pipeline.** A scheduled job that keeps both the archive and your
  analytics database current, with no per-survey code to maintain.

Three functions, one per stage:

| Function | Moves | What it does |
|----|----|----|
| `ingest()` | Alchemer → local `raw` | Downloads everything, verbatim. Later runs refresh only surveys that changed. |
| `pub_layer()` | local `raw` → local `pub` | Builds tidy, typed tables, plus a one-row-per-respondent view per survey. |
| `load_pub_layer()` | local `pub` → analytics DB | Copies `pub` into SQL Server, Postgres, or any other DBI backend. |

In ETL terms it is an **ELT** pipeline: data lands untouched first and
is transformed afterwards, which is what keeps a new question type from
ever breaking a run. If you’d rather think in medallion layers, `raw` is
bronze and `pub` is silver.

The package also exposes direct-API functions for one-off use, and keeps
the three functions from earlier releases as deprecated shims.

## Installation

``` r
# install.packages("devtools")
devtools::install_github("grousell/alchemeR")
```

## Configuration

Everything is read from environment variables, never from arguments in
your script. Copy
`system.file("extdata", "Renviron.example", package = "alchemeR")` to
`.Renviron` and fill it in:

    ALCHEMER_API_TOKEN=...
    ALCHEMER_API_SECRET=...
    ALCHEMER_DB=/srv/alchemer-db     # where the archive lives
    ALCHEMER_TZ=America/Toronto      # timezone for pub's timestamps

## Archive and analyse

``` r
library(alchemeR)

ingest()      # first run downloads everything; later runs refresh what changed
pub_layer()   # tidy typed tables + a wide view per survey
```

``` r
con <- alchemer_db(read_only = TRUE)

# one row per response x question, ready for dplyr
alchemer_tbl(con, "pub.answers") |> dplyr::collect()

# one row per respondent
survey_wide(con, "8611799")

DBI::dbDisconnect(con, shutdown = TRUE)
```

## Load into an analytics database

``` r
dest <- DBI::dbConnect(odbc::odbc(), Driver = "ODBC Driver 18 for SQL Server", ...)

load_pub_layer(dest, schema = "dbo")   # full overwrite, one table per pub table
load_pipeline_health(dest, schema = "dbo") # a row your monitoring can watch
```

`system.file("scripts", "etl_pipeline.R", package = "alchemeR")` is the
whole thing as a runnable script, ready for cron or Task Scheduler.

## Direct API access

``` r
alchemer_surveys()
alchemer_responses("8611799")
```

## Learn more

`vignette("getting-started")` to set up · `vignette("data-model")` for
the schema, history, and what is deliberately not stored ·
`vignette("scheduling")` for running it unattended ·
`vignette("troubleshooting")` for when a survey fails to refresh.
