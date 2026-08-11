# Alchemer Survey Ingestion Platform Specification

Version: 1.1
Status: Final

## Key Revision

The platform must not only ingest survey responses. It must preserve and publish all analyst-relevant information exposed by the Alchemer API, including survey metadata, question metadata, response data, response statuses, answer coding/reporting values where available, survey structure, pages, and supporting metadata required to interpret responses.

## Architectural Goals

- Reliable, reproducible ingestion.
- Automatic discovery of new surveys.
- No survey-specific logic.
- No row-level watermarking.
- Survey-level refreshes.
- Persistent local DuckDB application database.
- Publication into SQL Server analytics environment.
- Downstream transformation delegated to dbt and project-level analytics.

## Final Architecture

Alchemer API
    -> Local DuckDB application database
    -> SQL Server published survey tables
    -> dbt
    -> Reporting

DuckDB is the system of record for ingestion.

SQL Server is the system of record for analytics consumption.

## Technology Stack

### R

Single scheduled R script.

Responsibilities:

- API communication
- retries
- rate limiting
- survey discovery
- state management
- publication
- logging

### DuckDB

Persistent application database:

alchemer.duckdb

### SQL Server

Published survey tables only.

### Configuratiom

Minimal configuration is required. All by environment variables. Example Renviron file provided includes connection information. Default schema. Also, an alternate schema and list of survey ids that should be published to the alternate schema. A flag to enable publishing the final tables. Final tables are also stored in the application database regardless. Service account passwords are stored with the keyring package. 

## Data Model in DuckDB

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

## Refresh Strategy

### Chosen Strategy

Survey-level refresh.

Not response-level incremental loading.

Workflow:

1. Discover surveys.
2. Determine refresh candidates.
3. Download all responses for selected survey.
4. Download metadata for selected survey.
5. Rebuild survey-specific current-state records.
6. Publish survey outputs.

### Initial Refresh Rule

Refresh all surveys every run.

A future enhancement may use modified dates.

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

- rollback
- preserve prior state

### Logging

Database logging.

File logging.

## Publication Layer

### Principle

Minimal transformation.

The ingestion system is not a modeling layer.

### SQL Server Objects

Survey-specific response tables:

- alchemer_survey_<survey_id>

Shared metadata tables:

- alchemer_surveys
- alchemer_questions
- alchemer_question_options
- alchemer_pages
- alchemer_response_status_lookup (if applicable)

These metadata tables allow analysts to interpret survey-specific tables correctly.

### Survey Tables

One table per survey.

Structure should remain very close to Alchemer exports. Err toward "tidy data" where a choice of data shape must be made.

No survey-specific cleaning.

No business logic.

### Table Replacement Strategy

Publish via:

- staging table
- validation
- atomic replace

Never append directly to published survey tables. If surveys are removed from Alchemer, this is recorded but data is not removed from either the application db or published tables. 

Assume the publishing credentials are a restricted service account with write-only access to the configured schemas. 

## Deliverables

### Runtime Artifacts

- alchemer_refresh.
- example Renviron file for configuration
- alchemer.duckdb
- logs directory

### Runtime Behaviour

Scheduled execution:

1. Discover surveys.
2. Refresh survey metadata.
3. Refresh survey structures.
4. Refresh responses.
5. Publish survey tables.
6. Publish shared metadata tables.
7. Log results.

## Final Decision Summary

- Persistent DuckDB application database.
- Single R script implementation.
- Keyring for credentials.
- Alchemer API extraction.
- Optional ODBC publication to SQL Server.
- Survey-level full refreshes.
- No row-level watermarks.
- One published table per survey.
- Shared metadata tables for interpretation.
- Preserve all analyst-relevant API content.
