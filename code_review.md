# Code review — alchemeR 1.0.0 refactor

Reviewed at `worktree-refactor-1.0` (`b1e5fc7`, Phases 0–12 complete), ~2,000 LOC in
`R/`, ~1,400 LOC of tests, 3 vignettes, 2 example scripts.

## How this was verified

Not just read — exercised. Beyond `devtools::test()` (277 passing) and
`devtools::check()` (0 errors / 0 warnings / 0 notes), I drove the real code against
the fixture client and a real DuckLake database in a temp dir to confirm or refute
each suspicion. Findings below marked **verified** were reproduced, not inferred.
Two-process locking behaviour was tested with genuinely separate `Rscript`
processes, since DuckDB's file lock is per-process and an in-process test gives a
false negative.

## Verdict

The architecture holds up. The ADR-004 discipline (change detection as a hint, never
a filter) is respected everywhere I could find, including in the one place it would
have been easy to cheat — `run_integrity_checks(expected_count =)` is a check, not a
filter. `raw` really is all VARCHAR/JSON, integrity really is asserted post-write, the
deletion flagging works in both directions (survey and response), transaction rollback
is genuinely tested with an injected mid-transaction failure, and the test suite
contains real regression tests with the reasoning written down rather than
coverage-padding.

The problems that remain cluster in three places, and none of them are in the
extract or archival path:

1. **Timezone handling in `pub`** contradicts the project's own reference data
   (H1, H2) — the only finding that silently produces wrong values in a table
   analysts will read.
2. **The Load stage** is thinner than the Phase 11 checklist claims: no local record
   of what was loaded, and orphaned tables accumulate in the analytics database
   (M2, M3).
3. **Operational robustness against concurrent readers** is documented in the
   reassuring direction only; the damaging direction breaks scheduled runs (H3).

---

## Fixed directly in this review

| Fix | File(s) |
|---|---|
| `expunge()` left a complete, readable copy of the expunged data in `pub.responses`/`pub.answers` and its wide view — **verified**. Now removes the `pub` rows in the same transaction and drops the survey's wide views, with a regression test asserting the answer text is gone. | `R/maintenance.R`, `tests/testthat/test-maintenance.R` |
| `%||%` was taken from base R, which only has it from 4.4.0; nothing declared `Depends: R (>= 4.4)`. Now imported from `rlang` (already a dependency), so the package works on older R. | `R/alchemeR-package.R`, `NAMESPACE` |
| An optional setting present but blank in `.Renviron` (`ALCHEMER_RPM=`) bypassed its documented default: `Sys.getenv()`'s `unset` argument only applies when the variable is *absent*. Produced a base URL of `https:///v5` and `capacity must be a whole number, not a numeric NA` — neither naming the real problem. Since `Renviron.example` ships every optional setting as a `NAME=value` line, blanking one out is the natural way a user would unset it. New `env_or()` helper + test. | `R/config.R`, `tests/testthat/test-config.R` |
| `lintr::lint_package()` **crashed** (not linted — errored out), so the Phase 10 "lintr clean" criterion was never actually verifiable. Cause: a lintr 3.3.0.1 bug on `methods::setMethod(DBI::dbWriteTable, ...)` in `test-load.R`. File excluded with the reason recorded. Now: 1 benign false positive package-wide (`fixture_path` defined in a sibling helper file). | `.lintr` |
| `pub_layer()`'s wide-view title fallback used `%||%`, which does not catch `NA` — a survey with no title got the view name `wide_x_<id>` instead of falling back to the id. Now uses the package's own `or_default()`. | `R/pub_layer.R` |
| `db_check()`'s `@return` documented a `survey_id` argument the function does not have. | `R/db_check.R` |
| `etl_pipeline.R` labelled `ingest()` as "Extract + Transform" — wrong stage, and the opposite of what ADR-003 mandates. Now labelled Extract + Load / Transform / Load. | `inst/scripts/etl_pipeline.R` |
| `etl_pipeline.R` used `on.exit()` at the top level of a script, where it never fires — the destination connection was never closed, and leaked outright if a load threw partway. Now `tryCatch(finally =)`. | `inst/scripts/etl_pipeline.R` |

`devtools::test()`, `devtools::check()`, and `lintr::lint_package()` were re-run after
these changes.

---

## H1. `pub` treats unsuffixed timestamps as UTC; the project's own reference data says they are account-local

**Severity: high — silently wrong values in a table analysts read.** `R/pub_layer.R:60`

`utc_timestamp_sql()` converts `date_updated`, `created_on`, and `modified_on` as
UTC → America/Toronto. Three independent pieces of evidence say that is wrong:

1. **The package's own fixture** (`inst/extdata/fixtures/surveyresponse_list.json`,
   copied from Alchemer's documented example) has, for one response:

   ```
   date_started   2025-12-09 17:15:30 EST
   date_submitted 2025-12-09 17:15:37 EST
   date_updated   2025-12-09 17:15:43        <- no suffix
   ```

   A response updated 6 seconds after it was submitted. If `date_updated` were UTC
   it would read `22:15:43`. It is plainly on the same wall clock as the two
   EST-suffixed fields.

2. **The behaviour is verified as self-inconsistent.** Ingesting that shape and
   building `pub` gives:

   ```
   response_id date_submitted        date_updated
   101         2026-01-01 00:00:00   2025-12-31 19:00:01
   ```

   `date_updated` five hours *before* `date_started`, on the previous calendar day,
   for every response in the database. `date_updated > date_submitted` — a sanity
   check any analyst might reasonably rely on — is false everywhere.

3. **`docs/alchemer-api-reference.md`** (the snapshot this project treats as
   authoritative) says of `date_updated`: *"treat its timezone as
   ambiguous/configurable per account, not a hardcoded assumption."* The code
   hardcodes an assumption.

The vignette and the function comment both document the assumption honestly and note
that a mismatch means "a fixed number of hours off, not scrambled" — that reasoning is
right, but it was used to justify picking the option the evidence points *away* from.

**Not an ingest correctness problem:** change detection compares the verbatim `raw`
strings, so ADR-004 is unaffected and `raw` is untouched. This is a `pub`-typing bug
only, and re-running `pub_layer()` fixes it retroactively once the conversion changes.

**Recommended fix.** Treat unsuffixed timestamps as being in the same zone the
EST/EDT-suffixed fields already reveal (i.e. no shift), and make it configurable
rather than assumed:

```r
# config.R
alchemer_tz <- function(tz = NULL) tz %||% env_or("ALCHEMER_TZ", "America/Toronto")

# pub_layer.R -- unsuffixed fields are account-local wall clock, per Alchemer's
# own documented example, so there is nothing to convert.
local_timestamp_sql <- function(column) glue::glue("TRY_CAST({column} AS TIMESTAMP)")
```

Keep `est_edt_timestamp_sql()` as it is — parsing the explicit suffix is correct and
worth keeping rigorous. Add a `pub.responses` test asserting
`date_updated >= date_started` for the documented fixture; that single assertion would
have caught this.

## H2. `America/Toronto` is hardcoded in a community-published package

**Severity: high — wrong for every user outside Eastern time.** `R/pub_layer.R:45-64`

`pub_layer()` normalises every `pub` timestamp to `America/Toronto`, chosen because
"the userbase (students at a Toronto institution) is single-timezone". But this is a
public package: `DESCRIPTION` and `README` describe it as a general Alchemer client,
`ALCHEMER_DOMAIN` supports four regions, and `NEWS.md` sells "accounts outside the
Canadian region now work correctly" as a headline fix. A German or Australian user
gets their timestamps silently shifted into Toronto wall-clock time with no setting to
change and no error.

**Recommended fix.** Same `ALCHEMER_TZ` knob as H1 (defaulting to `America/Toronto`
preserves current behaviour for the primary user), plus a `pub_layer(tz =)` argument,
and a line in `vignette("data-model")` under the timestamps section.

## H3. A read-only reader blocks the scheduled writer, and nothing retries

**Severity: high — a single idle analyst session kills every scheduled ETL run.**
`R/db.R:67-90`, `vignettes/scheduling.Rmd:67-88`

**Verified** across two separate processes:

| Already attached | New `read_only = TRUE` | New read/write |
|---|---|---|
| read/write | fails (`alchemeR_db_locked`) | fails |
| **read-only** | **ok** | **fails** |

The vignette documents the first row and the "many readers coexist" fact, and frames
the consequence reassuringly: *"expect an analyst's read to occasionally fail with a
clear 'try again shortly' message."* It never mentions the bottom-right cell, which is
the one that hurts: an analyst who leaves `con <- alchemer_db(read_only = TRUE)` open
in an RStudio session — the exact thing `alchemer_db()`'s `read_only` argument invites,
and which `vignette("getting-started")` demonstrates — makes **every** subsequent
`ingest()` abort. On a `*/15` cron that is a silent outage until someone notices, and
`inst/scripts/etl_pipeline.R` has no retry: `ingest()` aborts, the script dies, and
nothing loads.

This is also the Phase 12 item *"Robust to analytics reads of either application or
analytics databases during ETL pipeline runs"* — it is not met, and the deviation
isn't recorded anywhere.

Worth noting alongside: `refresh_survey()` deliberately opens its write transaction
*before* any network call (`R/ingest.R:187`, correctly, so a fetch failure reaches the
rollback path), and `load_pub_layer()` holds a read-only connection open for the whole
duration of the copy to the analytics database. Both widen the window in which the
lock is held.

**Recommended fix.** Bounded retry with backoff on lock acquisition, since the
failure is transient by nature:

```r
alchemer_db <- function(db = alchemer_db_path(), read_only = FALSE,
                        lock_wait_s = if (read_only) 0 else 300) { ... }
```

Retry only on the `Could not set lock on file` branch, and abort with the existing
message once the budget is exhausted. Then say plainly in `scheduling.Rmd` that a
held read-only connection blocks writers, so analysts should disconnect rather than
leaving a session attached, and cron jobs should be given a lock budget longer than a
typical interactive read.

---

## M1. Turning an `include =` dataset off deletes what was already archived

**Verified.** `R/ingest.R:194-215`, `R/db_schema.R:179`

`refresh_survey()` builds an empty tibble when a dataset is not in `include`, then
passes it to `replace_survey_rows()`, which `DELETE`s the survey's existing rows
before writing nothing:

```
run 1, include = c("campaigns", "statistics")  ->  campaigns: 1  statistics: 1
run 2, include = character(0)                  ->  campaigns: 0  statistics: 0
```

So `ingest(include = "campaigns")` — a plausible thing to do when statistics fetches
start failing, or just to save requests — permanently destroys previously archived
statistics for every survey it refreshes. That contradicts the archival premise and
the flag-don't-delete rule; a dataset the run didn't even ask about should be inert,
not wiped.

**Recommended fix.** Skip the table entirely rather than replacing it with nothing:

```r
if ("campaigns" %in% include) {
  replace_survey_rows(con, "survey_campaigns", survey_id, stamp(campaigns))
}
```

and document that omitting a dataset leaves whatever was last ingested in place. Add
a test for the two-run sequence above.

## M2. Renaming a survey orphans its wide table in the analytics database, forever

**Verified.** `R/load.R:43-63`, `R/pub_layer.R:186-234`

`pub_layer()` correctly drops a survey's old `wide_*_<id>` view locally when its title
changes. `load_pub_layer()` has no equivalent: it only ever overwrites tables that
currently exist in `pub`.

```
dest before rename: ..., wide_customer_feedback_q1_1
local pub after:    ..., wide_customer_feedback_q2_1
dest after reload:  ..., wide_customer_feedback_q1_1, wide_customer_feedback_q2_1
```

The stale table stays in the warehouse indefinitely, still queryable, frozen at the
last load before the rename, with nothing marking it stale. Downstream dashboards
pointing at it keep working and keep showing old data — the worst failure shape for
an analytics warehouse. The same applies to a survey that is expunged locally: its
tables persist downstream.

**Recommended fix.** This needs the load-state table from M3. Record the table names
written on each successful load in `meta.loads`; on the next load, drop destination
tables that this pipeline previously wrote and no longer produces. Scoping the drop
to *previously recorded* names is what makes it safe — never "drop anything matching
`wide_%`", which could hit a table the pipeline doesn't own. Until then, document it
in `vignette("scheduling")` next to the existing full-overwrite trade-off paragraph:
renaming a survey requires manually dropping the old table downstream.

## M3. The Load stage keeps no state in the application database

`R/load.R`, `plan.md` Phase 11

Phase 11 asks for *"Tracks state exclusively in local application database so that the
service account to analytics db can be (over)write-only."* The write-only half is real
and well-tested (`test-load.R` proves it with a connection wrapper that throws on any
read — a genuinely good test). The tracking half isn't there: neither `load_pub_layer()`
nor `load_pipeline_health()` writes anything to `ALCHEMER_DB`. Consequences:

- Nothing in the application database can answer "did last night's load succeed?" —
  `db_status()` reports the *ingest* run, and `load_pipeline_health()` writes only to
  the destination, which the pipeline cannot read back by design.
- `load_pipeline_health()` reports `db_status()`, which contains no load information
  at all. A monitor watching that table sees a healthy ingest and cannot tell that
  the Load stage has been failing for a week.
- M2 has nowhere to record what it wrote.

**Recommended fix.** A `meta.loads` table (`load_id`, `started_at`, `finished_at`,
`status`, `destination`, `n_tables`, `n_rows`, `tables` as JSON), written by
`load_pub_layer()` outside any destination transaction (ADR-007's reasoning applies
here too), and surfaced in `db_status()` so `load_pipeline_health()` carries it
downstream.

## M4. `meta.integrity_checks` can only ever contain passes

`R/ingest.R:218-225`

Checks are computed, and if any fail, `stop()` fires *before* the
`write_rows_generic(con, "meta.integrity_checks", ...)` call — and the rollback would
discard it anyway. So the table records successes only. The failure that most needs a
durable record is the one that leaves none, which makes the table actively misleading
for monitoring: "no failed rows in `meta.integrity_checks`" does not mean "no failed
checks". The reason lands in `meta.run_events.message` as free text only.

ADR-007's own logic covers this ("failure records must outlive the failure") and
`update_survey_state()` already does exactly the right thing three lines further down
— write after the rollback, in autocommit. The integrity checks should follow it.

Also: `db_check.R:1-5`'s comment says `ingest()` "persists them to
`meta.integrity_checks` after every survey commit". It writes them *before* the commit,
inside the transaction.

**Recommended fix.** Return `checks` from the tryCatch and write them after
`COMMIT`/`ROLLBACK`, whichever ran.

## M5. ETL terminology

You asked for this specifically. The scripts are fixed (see above); what remains is
conceptual, in prose.

**What the pipeline actually is:** Extract (Alchemer API) → Load (`raw`, verbatim and
untyped) → Transform (`pub`, in place) → Load (analytics database). The first three
stages are **ELT**, deliberately: ADR-003 forbids transforming before landing, and
that is the design's main safety property, not an implementation detail. Calling the
whole thing "an ETL pipeline" is fine as a label for the end-to-end job — that is
ordinary industry usage and it is what the user asked for — but the docs should not
imply `ingest()` transforms.

- `README.Rmd:29` — "`load_pub_layer()` completes the ETL pipeline" is accurate for
  the whole job. Consider one clarifying clause: the local stages are ELT (land
  verbatim, transform in place), and `load_pub_layer()` is the final Load.
- `README.Rmd` has no Load example even though it advertises the capability; every
  other function gets a snippet. Worth three lines.
- `db_status()` is described in `load.R:71` as a "pipeline-health summary". It is an
  *ingest* health summary (see M3).
- `vignette("data-model")` never uses the words extract/load/transform at all, which
  is fine for a schema reference, but the `raw`/`pub` split is exactly the
  bronze/silver distinction and one sentence naming it would help anyone arriving
  from a data-engineering background.
- Correct and worth keeping: `load.R`'s header calling itself "the 'L' of an ETL
  pipeline"; "full overwrite, never an incremental upsert"; the explicit
  idempotency claim.

---

## Low

- **`meta.runs.n_requests` is always `NULL`** (`R/ingest.R:285,373`). A declared
  operational metric that nothing populates — and it is the metric Phase 6's
  acceptance criterion is stated in terms of ("1 discovery + 1 probe/survey + ~5 per
  refreshed survey"). Either count requests on the client (a counter in the
  `alchemer_client` environment, incremented in `alchemer_perform()`) or drop the
  column; shipping an always-null column invites someone to trust it.
- **`raw.contacts` and `raw.contact_lists` are created but never written** by any code
  path (`R/db_schema.R:47-52`). `?ingest` says contacts "are not fetched by `ingest()`
  at all yet", so these ship as permanently empty tables. Either drop them until the
  opt-in exists or note in `vignette("data-model")` that they are reserved.
- **Uniqueness is asserted for responses only.** `run_integrity_checks()` checks
  duplicate `(survey_id, response_id)` but nothing for `raw.surveys.survey_id` or
  `(survey_id, question_id)` / `(survey_id, question_id, option_id)`. Since ADR-006
  exists precisely because DuckLake has no constraints, and `upsert_surveys()` writes
  `raw.surveys` wholesale, the survey-level check is cheap and worth adding.
- **`json1()`'s comment is wrong** (`R/parse.R:24-26`): it says `NULL` and `list()`
  both become SQL NULL "not the string `null`", but only `NULL` returns
  `NA_character_`; `list()` serialises to `"[]"`. Confirmed —
  `raw.responses.survey_data` holds `[]` for a response with no answers. The
  behaviour is defensible (an empty array *is* what the API sent); the comment
  should say so.
- **`random_suffix()` consumes the caller's RNG stream** (`R/db_schema.R:155`). Every
  `ingest()` silently advances the user's random seed, so a script that sets a seed
  and then ingests gets different draws than one that doesn't. Use
  `basename(tempfile(""))` or wrap in `withr::with_preserve_seed()`.
- **`alchemer_db()`'s lock message overstates the restriction** (`R/db.R:76-80`):
  "this blocks a second reader just as much as a second writer". Verified false —
  two read-only connections coexist fine (`scheduling.Rmd` says so correctly). Only a
  *writer* blocks, or is blocked.
- **`ingest(dry_run = TRUE)` is documented as writing nothing**
  (`R/ingest.R:268`, `vignettes/getting-started.Rmd:46`) but it creates the database
  directory, all 25 tables, and ~25 DDL snapshots — it writes no *data*. Since
  `dry_run` is the recommended first command a new user runs, the distinction is
  worth a word.
- **`ingest(surveys = "<id that doesn't exist>")` reports success**: `status = "ok"`,
  `n_fetched = 0`, and it writes a `meta.survey_state` row for a survey that isn't
  in the account. Against the real API this would error, so it is low-risk, but a
  membership check against `discovered$survey_id` with a clear message would be
  kinder than an inscrutable integrity failure or a phantom state row.
- **`page_is_complete()` parses each response body twice** (`R/api.R:136`, then
  `R/api.R:185`) — once to test completion, once for the payload. Harmless at this
  scale; noted only because a large survey's pages are re-parsed for nothing.
- **One remaining lint**, a false positive: `object_usage_linter` cannot see
  `fixture_path()` (defined in `helper-fixtures.R`) from `test-parse.R`.

---

## Questions

1. **H1/H2 — timezones.** Do you want me to make the change? My reading of the
   evidence is that unsuffixed Alchemer timestamps are account-local, not UTC, which
   makes `pub.responses.date_updated` currently wrong by 4–5 hours for every row. The
   fix is small (one SQL helper plus a config knob) and `pub` is rebuildable, so
   nothing is lost by changing it. I stopped short because it changes the meaning of
   stored values, and because confirming it against your account takes one API call
   that I can't make: fetch any response and compare `date_submitted` to
   `date_updated`. If they read within seconds of each other, the UTC assumption is
   wrong.
2. **H3 — lock contention.** Is a retry budget on `alchemer_db()` the behaviour you
   want, or would you rather the scheduled job fail loudly and be retried by cron?
   Retrying inside the package is friendlier but hides contention; failing fast keeps
   the signal.
3. **M2/M3 — `meta.loads`.** Worth building, or is dropping stale downstream tables
   by hand acceptable at your scale? It is the one finding that adds a table and real
   code rather than tightening what's there.
4. **Wide views in the warehouse.** Loading them makes the destination table set
   change whenever a survey is renamed (M2). Would you rather load only the five
   fixed `pub` tables and let the warehouse pivot, keeping the destination schema
   stable?
