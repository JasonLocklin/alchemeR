# Alchemer Survey Ingestion Platform Specification

Version: 2.0
Status: Final
Alchemer API documentation: https://apihelp.alchemer.com/help/api-reference

## Project Refactor Context

### CURRENT STATE
Currently, users use this package in their analysis R scripts to query the
Alchemer API and pull current data into their R session. This makes all analysis
scripts dependent on continued access to the Alchemer survey API (migrating away
from Alchemer or deleting a survey from the platform will break scripts). Data
loss prevention is entirely dependent on Alchemer. This is also slow and
awkward, as users must wait for the rate-limited and pagenated data downloads
whenever updating their data. The package uses a hacky page to file, then load
the data in the script process to assist with this.

### INTENDED STATE
- User schedules an R script with alchemeR::ingest(), configured via environment
variables and all Alchemer data available to the account is saved efficiently
locally in an open data format (consider this an application database). Then
data engineers can process/clean/pipeline that date into a gold level of their
shared analytics database OR analysts can directly query that open data format
for their survey data in their analysis scripts and do their own data cleaning
and processing. 
- alchemeR::ingest() is safe to run, which means a first run (that
downloads all data) is done so politely and avoids API rate limiting, and
subsequent runs check for surveys with new data and only pull data from those
surveys, allowing high frequency schedules without exploding the API requests,
bandwidth, or data storage requirements. 
- alchemeR::ingest() stores all user's available data in the application database with
the highest fidelity practical. Users should never find themselves needing to use the API
to query some information that was neglected to be stored in the application database, and 
troubleshooting any data processing should be possible within the applicaton database directly because
the source material is available. This is a ingestion system and not a modeling layer.
- Most users, most of the time, will simply want to pull the "current response data" from a given survey from the application
database (like they do now with the API). The application database archetecture should make this type of query simple and efficient.
- Existing project functions will continue to be available for users wishing to download data directly
from Alchemer via the API. However, this is no longer the primary use case, and breaking changes to 
these functions are encouraged in order to meet the refactor goals cleanly and efficiently. We will increment
the package major version and document breaking changes appropriately. 
- Overall package structure and files are cleaned up and meet current best practices.

#### Use Cases:

Data Engineers:
    - Schedules a simple script that uses alchemeR functions to injest the data, build the publication layer tables, and then pushes the latter to "source" tables on an analytics database. They may use dbt after that to do more detailed transformations or cleaning, including survey-specific logic to produce analytics marts.

R Analysts:
    - May Schedule a script to maintain application db locally on their laptop or on a shared fileserver, or may just consume that application db from a fileserver maintained by someone else.
    - Writes analytics scripts that query tables in the application db (typically either the publication layer) as the first step in their data cleaning and preperation of survey data before analysis.
    - Rarely: may use the archival layer tables to investigate data quality concerns or find data from a particular time in the past. 
    - Also rarely: May also use the other package functions direclty to query the survey data via the API to their R session for troubleshooting or querying the absolute most up-to-date survey results (e.g., monitoring response counts throughout the day durring a big data collection).


## Application database REQUIREMENTS

- All data and metadata is preserved in the application database so that:
    1. Loss of Alchemer account never leads to loss of analytics capacity (data loss or loss of critical information to (re)process older surveys).
    2. Analysts and data engineers never need to use the Alchemer API for their work. Everything is available from the application database.
- Must be a long-term supported local on-disk open data format and read/write of tables and compute will be done via duckdb. 
- Users must be able to query survey data at a past date and retrieve the response data available at that date (consider an open data format that supports historical versions/snapshots natively).

## Key context that impact script and application database archetecture decisions
- In a typical user's environment (small scale research departments), most of the Alchemer surveys are static and will have no new daily data (they may be closed, or may simply not be actively promoted). 
- Expect the data scale to be less than 100 surveys, most with hundreds of responses, and a few surveys with response counts in the order of 100k responses.
- Departments may be requred to delete or expire survey data on Alchemer's surveys and this should not cause data loss in the application database.
- Alchemer tables are NOT immutable. A response in progress will become changed as the survey-taker is completing the survey. A closed window may result in a survey response closing days later.
- Big survey pushes may result in 10s of thousands of responses a day in one survey, while other surveys remain steady.
- Some long-running surveys may get 1 or 2 responses a day, and should not baloon data storage over time.
- Unintended data loss or corruption is the largest concern.
- Everything should be datestamped, and the archetecture design should permit automatically expunging research data past a certain age or date. 
- Alchemer is aggressive at protecting their API with pagenation and rate limits. alchemeR::ingest() is intended to be a scheduled job, so can afford to be polite.

## Refactor Goals

- Reliable, reproducible ingestion. Boring infrastructure.
- Minimal, maintainable, auditable code. Use only well-established libraries and minimal LOC to achieve requirements, and use good, succinct commenting practice.
- Automatic discovery of new surveys.
- No survey-specific logic.
- No row-level watermarking (response rows are mutable).
- Survey-level refreshes via API.
- Persistent local application database.
- application database provides complete archive of alchemer data

## Technology Stack


### R
All R code. Minimal LOC to achieve the end goals in production quality,
auditable, and maintainable tool. Reliability is a requirement. Maintainable is
a requirement.
Keep existing package functions available to R users who may want to pull data
directly from Alchemer in their scrips, bit they should be audited and
refactored where necissary to meet this spec.

Responsibilities:

- API communication
- retries
- rate limiting
- survey discovery
- state management
- logging in application database (monitoring of out of scope, but users may monitor these tables for failures themselves).

Application database is an open data format and is exclusively interacted with via Duckdb in R code. Using dbplyr is encouraged rather than embedded SQL strings
where it can accomplish the same job more readably. 


### Configuration for alchemeR::ingest()

Minimal configuration is required using environment variables. Example
Renviron file provided includes connection information. API password can be provided
directly via argument or environment variable as some users may use the keyring package 
to provide service account password. Other functions should follow this pattern to avoid configuration
details ending up on users project sourcecode. 

## Proposed Data Model for Application Database

This is only a suggestion. Prioritize above requirements and goals. 

### survey_inventory

One row per discovered survey.

Columns include:

- survey_id
- title
- subtype
- status
- created_date
- modified_date
- last_refresh
- last_successful_refresh

### refresh_log

One row per survey refresh attempt.

### raw_api_payloads

Optional but recommended.

Stores raw JSON payloads retrieved from Alchemer.

Used for:

- audits
- troubleshooting
- replayability

### survey_metadata

Current survey-level metadata.

### survey_pages

Page definitions for surveys.

### survey_questions

Question definitions and metadata.

Includes everything needed to interpret responses.

Examples:

- question_id
- title
- type
- shortname
- reporting settings
- page association
- option definitions

### survey_question_options

Stores coded values and selectable options.

Examples:

- survey_id
- question_id
- option_id
- option_title
- reporting_value
- sku/value if available

This table is important because many surveys rely on coded responses.

### current_responses

Current-state responses.

One row per response.

Stores:

- survey_id
- response_id
- response status
- timestamps
- response payload

### response_answers

Normalized response-answer representation.

One row per answer.

Examples:

- survey_id
- response_id
- question_id
- answer_value
- answer_text

Used to create publication tables.

## What Must Be Retained From Alchemer

The guiding rule is:

Any information available through the API that could later be useful to an analyst should be preserved.

This includes:

- survey definitions
- survey metadata
- survey status
- survey type
- question metadata
- pages
- option lists
- coded values
- reporting values
- response statuses
- timestamps
- statistics where available
- contact/campaign-related metadata if exposed and accessible to the account

The ingestion process should favor retention over filtering.

A layered strategy may be implemented with a high-fidelity achival layer, a "current state" layer
that provides the current state of the data in a high-fidelity-to-Alchemer-api structure. A seperate function may create a 
publication layer that does light transformation, simplfication, and filtering to prepare the source data
for exporting to an analytics database for data engineering pipelines and/or R analysists data cleaning and analysis scripts that does not require
high fidelity with the Alchemer api data structures, but would benefit from cleaner, simplified source tables. 

## Refresh Strategy

### Chosen Strategy

Survey-level refresh.

Not response-level incremental loading.

Workflow:

1. Discover surveys.
2. Determine refresh candidates.
3. Download all responses for selected survey.
4. Download metadata for selected survey (politely).
5. Rebuild survey-specific current-state records.


## Reliability Requirements

### Retries

Retry:

- 429
- 500
- 502
- 503
- 504

Exponential backoff.

### Transactions

Every survey refresh executes inside a transaction.

Failure:

- rollback if required
- preserve prior state

### Missing data

If surveys or rows are no longer available via the API (e.g., deleted responses or deleted surveys), ensure the appropriate records are marked as such in the application
database, but do not automatically remove them from any layer. Downstream users may use your flag to exclude deleted responses, but that would be up to them.

## Publication Layer

### Principle

Data lightly transformed, filtered, and simplified for export to analytics database as "source" data. This is the only layer that does not 
require archival level fidelity to Alchemer's data structures available via the API. It does not intend to replace a data engineering pipeline or data analysts
data cleaning and transformation work. It is intended to allow for the earlier layers to maximise archival-grade fidelity to the complete raw Alchemer API data structures,
while providing more convenient "source" tables of survey data to downstream users without that constraint.

Layer is built with a seperate function - alchemeR::pub_layer() to optionally add this minimal processing while keeping the ingestion function simple. Some users may be using this 
package for archiving data only, so may not need this layer computed. 

Goals:
    - Easy consumption by R users (err toward "tidy data", with few or no metadata/dimension tables to join each survey where practical). 
    - Sufficient data for data engineering pipelines without administrative cruft un-necissary for downstream analytics.
    - Easy to write a query that pulls all publication tables to make a push-to-analytics-db script easy to write (dedicated schema?). 
    - Naming scheme is clear to human analysts and data engineers.
    - No survey-specific logic, must be universal processing.
    - Must be trustworthy to consumers without having to audit the archival level tables.




