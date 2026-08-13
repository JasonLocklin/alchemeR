# Code review — alchemeR 1.0.0 refactor

Reviewed at `worktree-refactor-1.0` (`b1e5fc7`, Phases 0–12 complete), ~2,000 LOC in
`R/`, ~1,400 LOC of tests, 3 vignettes, 2 example scripts.

> **Status: all findings resolved.** Everything below was fixed across three commits
> (`2a00bec`, `64aa5ea`, `b545974`) plus a documentation pass. Each section records what
> was decided and what changed; the review is kept as the record of *why*. Test suite:
> 277 → 332 passing.

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

**Fixed** (`64aa5ea`). Unsuffixed timestamps are now cast with no shift, since they are
already account-local. `est_edt_timestamp_sql()` still parses the explicit suffix into
an exact instant — that part was right — and now renders it in the configured zone. A
test asserts `date_updated > date_started`, the property that was false for every row;
another proves `raw` is untouched whatever `tz` is used.

## H2. `America/Toronto` is hardcoded in a community-published package

**Severity: high — wrong for every user outside Eastern time.** `R/pub_layer.R:45-64`

`pub_layer()` normalises every `pub` timestamp to `America/Toronto`, chosen because
"the userbase (students at a Toronto institution) is single-timezone". But this is a
public package: `DESCRIPTION` and `README` describe it as a general Alchemer client,
`ALCHEMER_DOMAIN` supports four regions, and `NEWS.md` sells "accounts outside the
Canadian region now work correctly" as a headline fix. A German or Australian user
gets their timestamps silently shifted into Toronto wall-clock time with no setting to
change and no error.

**Fixed** (`64aa5ea`). New `ALCHEMER_TZ`, with a `pub_layer(tz =)` / `expunge(tz =)`
argument, validated against `OlsonNames()` whether it arrives from the environment or an
argument (it reaches SQL as a bare literal). It defaults to the machine's own timezone
rather than to Toronto, and the docs say to set it explicitly for scheduled runs — an
unset default that varies by machine would otherwise make `pub`'s values depend on which
host ran `pub_layer()`. `Renviron.example` ships `America/Toronto` as the example value.

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

**Fixed** (`b545974`), with the policy the user chose: a reader is waited out for
`lock_wait_s` (default 300) and then killed, because it holds no writes and cannot
corrupt anything; a concurrent *writer* is never waited out or killed, since it may be
partway through a refresh transaction.

The two are told apart by `writer.lock`, which records the attaching writer's PID. What
makes this robust is that the file's *existence* isn't the test — the PID in it is
compared against the PID DuckDB names as actually holding the lock. A writer that
crashed leaves the file behind but holds nothing, so the stale file self-heals instead
of blocking every future run until someone deletes it by hand. `break_lock = FALSE`
opts out of killing.

Tested across genuinely separate `Rscript` processes, which is the only way this can be
tested: DuckDB's lock is per-process, so two attachments from one R session never
conflict and an in-process test would pass for something that fails in production.
`scheduling.Rmd` now documents both directions in a table.

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

**Fixed** (`b545974`). The table is skipped entirely rather than replaced with nothing,
so omitting a dataset means "don't fetch it this run", never "delete it". A test runs the
two-run sequence above.

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

**Fixed** (`b545974`), using the `meta.loads` table from M3. Each load records the
destination tables it wrote; the next load drops those it recorded and no longer
produces. The drop is scoped to *previously recorded* names — never a pattern sweep like
`wide_%`, which could hit a table the pipeline doesn't own — and a test asserts that a
same-shaped table the pipeline didn't create survives. Wide views keep their readable
titled names, per the user's call.

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

**Fixed** (`b545974`). New `meta.loads` (`load_id`, `started_at`, `finished_at`,
`status`, `destination`, `n_tables`, `n_rows`, `tables` as JSON, `message`), written by
`load_pub_layer()` on success *and* failure, and surfaced in `db_status()` so
`load_pipeline_health()` carries it downstream — a monitor watching the analytics
database can now tell a stalled extract from a stalled load. A failed load also raises
(`alchemeR_load_error`) instead of returning quietly.

The copy itself still runs over a read-only connection, so it doesn't hold the write
lock for the duration; `meta.loads` is written afterwards over a short read/write one.
`destination` records the driver class, never a connection string, which on several
backends carries credentials.

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

**Fixed** (`b545974`). `checks` is now written after `COMMIT`/`ROLLBACK`, whichever ran,
so failures are recorded. The regression test needed a failure the refresh couldn't
erase — anything injected into `raw` beforehand is replaced before the checks run — so it
uses a list endpoint returning the same response twice, which is what an overlapping page
would look like, and is exactly the case DuckLake cannot declare a constraint against.

## M5. ETL terminology

**What the pipeline actually is:** Extract (Alchemer API) → Load (`raw`, verbatim and
untyped) → Transform (`pub`, in place) → Load (analytics database). The first three
stages are **ELT**, deliberately: ADR-003 forbids transforming before landing, and that
is the design's main safety property, not an implementation detail. Calling the
end-to-end job "an ETL pipeline" is ordinary usage and fine; what the docs must not do is
imply `ingest()` transforms — which `etl_pipeline.R` did, heading it "Extract +
Transform".

**Fixed** across the commits and the documentation pass:

- The scripts now name each stage correctly (Extract + Load / Transform / Load), with a
  one-line note that nothing is transformed at ingest and why.
- `README` states the stage table up front, names the pattern as ELT with the reason in
  one clause, and gives the bronze/silver equivalence for readers arriving from data
  engineering — then gets straight to code. It now has a Load example.
- `vignette("data-model")` opens with a schema-to-stage table, so the ETL vocabulary and
  the `raw`/`pub`/`meta` names are introduced together.
- `db_status()` genuinely is a pipeline-health summary now that it covers both stages
  (M3), so `load.R`'s description became true rather than being reworded.
- `vignette("scheduling")` was retitled "Running the pipeline unattended" — it covers the
  Load stage and monitoring, not just `ingest()`.

Prose throughout was cut for length at the same time: the wordier passages were
explaining decisions at a length that belonged in code comments (where most of them
already are) rather than in a user-facing document.

---

## Low — all fixed in `b545974`

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

## Decisions taken

1. **Timezones (H1/H2).** `raw` stays whatever Alchemer sent; `pub` is local time, set by
   `ALCHEMER_TZ` rather than hardcoded. Worth confirming against the live account once
   credentials exist — fetch any response and compare `date_submitted` with
   `date_updated`. Reading within seconds of each other confirms the new behaviour;
   five hours apart would mean the old UTC assumption was right after all.
2. **Lock contention (H3).** Writers record a lockfile and a second writer fails fast; a
   reader is waited out 5 minutes and then killed as stuck.
3. **Load state (M2/M3).** `meta.loads` built, with scoped cleanup of stale destination
   tables.
4. **Wide views** keep their titled names downstream; the `meta.loads` cleanup is what
   handles renames.

## Still worth knowing

- **The reader-kill is destructive to a person, not to data.** An analyst who leaves a
  read-only session open past the 5-minute budget loses it with no warning at their end.
  `getting-started.Rmd` now tells analysts to disconnect when done, which is the only
  mitigation that doesn't reintroduce the blocking problem.
- **Nothing here has run against the real API.** Every finding above was verified against
  fixtures and a real local DuckLake database, but no Alchemer credentials exist in this
  environment (ADR-010). The first live run is still the first live run — `ingest(dry_run
  = TRUE)`, then one small survey, then `db_check()`, as `getting-started.Rmd` sets out.
- **One lint remains**, a false positive: `object_usage_linter` cannot see
  `fixture_path()` in `test-parse.R` because it's defined in a sibling helper file.
