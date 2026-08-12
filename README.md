
<!-- README.md is generated from README.Rmd. Please edit that file -->

# alchemeR

<!-- badges: start -->

[![R-CMD-check](https://github.com/grousell/alchemeR/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/grousell/alchemeR/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

alchemeR downloads your whole [Alchemer](https://www.alchemer.com/)
account into a local **DuckLake** application database, so analysis
scripts stop depending on continued access to the Alchemer API.
`ingest()` is safe to schedule: a first run downloads everything
politely, and subsequent runs only refresh surveys that have actually
changed. Once ingested, `pub_layer()` builds tidy, typed tables (and a
one-row-per-respondent wide view per survey) for ordinary analysis.

The package also still exposes direct-API functions for one-off use, and
preserves the three functions from earlier releases as deprecated shims.
See `vignette("data-model")` for the schema and
`vignette("getting-started")` to begin.

## Installation

You can install the development version of alchemeR from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("grousell/alchemeR")
```

## Configuration

alchemeR reads credentials and settings from environment variables (see
`inst/extdata/Renviron.example`), never from arguments in your script:

``` r
Sys.setenv(
  ALCHEMER_API_TOKEN = "...",
  ALCHEMER_API_SECRET = "...",
  ALCHEMER_DB = "~/alchemer-db"
)
```

## Example

### Ingest the account, then build the publication layer

``` r
library(alchemeR)

ingest()      # first run: downloads everything; later runs only refresh changes
pub_layer()   # typed tables + one wide view per survey
```

### Query the application database

``` r
con <- alchemer_db()
dplyr::collect(alchemer_tbl(con, "pub.responses"))
survey_wide(con, "8611799")
DBI::dbDisconnect(con, shutdown = TRUE)
```

### Direct API access, for one-off use

``` r
alchemer_surveys()
alchemer_responses("8611799")
```
