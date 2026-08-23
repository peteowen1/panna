# panna 0.3.28 (dev)

## Tiento for every domestic + cup club, not just the World Cup 48 (panna#193)

New step `12d_export_domestic_team_strength.R` publishes `team_strength.parquet`
(one row per team: `panna, offense, defense, epr, psr, elo, tiento, rank_tiento,
squad_n, n_rated, elo_seeded, seed_method, is_domestic_league, build_id`) for
every club in the current fixture window -- domestic-league clubs and cup-only
clubs from leagues panna doesn't scrape (e.g. Champions League opponents,
~253 teams at spec time). Squad aggregation mirrors `12_export_wc2026_blog.R`
section 5 exactly (`build_team_expected_minutes(international_only = FALSE)`,
same minutes-weighted formula, same rating sources), and Tiento reuses the
existing weights (`panna .40 / epr .20 / elo .30 / psr .10`) unmodified,
z-scored across the GLOBAL pool per `DOMESTIC-TIENTO-2026-08-21.md`. A
dual-labelled team (domestic league + a continental cup this season, ~19 of
them) is pinned to its domestic league.

Elo is the one genuinely new piece: a team freshly promoted from a division
panna doesn't scrape (currently Le Mans FC from Ligue 2, SV 07 Elversberg from
2. Bundesliga -- confirmed against the live `predictions-latest` release,
which shows exactly these two and no others with NA Elo in the current
fixture window) has no tracked history at all, and `03_team_rolling_features.R`
deliberately surfaces that as NA rather than falling back to `ELO_INITIAL =
1500`. This step seeds such a team at the mean final Elo of the teams
relegated from that league the previous season -- measured near-unbiased
(mean error +15, MAE 69, sd 89 across 57 clean league-seasons) versus the
league mean (179 MAE) or ELO_INITIAL=1500 (97 MAE), so no correction term.
Falls back to
the league mean (and says so in the log) when no relegated cohort is
identifiable -- a league's first tracked season, or a cup-only team (a cup has
no relegation concept). `elo_seeded` marks every seeded team; `seed_method`
records which rule fired. Non-fatal in the pipeline (`run_pred_step_optional`,
step 12d) -- a failure here must never hold back predictions-latest or
blog-latest, same reasoning as step 12.

## A missing events table no longer discards an entire season's matches (panna#166)

`01_build_fixture_results.R` loaded `events` before `fixtures` in its per-
`(league, season)` loop, and the whole block is wrapped in a `tryCatch` that
only `message()`s on error. When the derived `opta_events.parquet` table is
missing for one season, `load_opta_events()` threw before
`load_opta_fixtures()` was ever reached, and the outer handler silently
dropped every match for that season — on every run.

Confirmed live for AFCON "2021 Cameroon": 52 fixtures and 2,320 lineup rows
in the Opta data, 86,748 raw events in `events_AFCON.parquet`, but 0 rows in
the derived `opta_events.parquet` (the raw events were never derived into the
smaller table — a pannadata-side gap, not a scrape failure or a label
mismatch). Every one of those 52 matches vanished from the results dataset
each time the predictions pipeline ran.

`events` is a fallback — the code's own comments call `opta_fixtures.parquet`
the primary source for scores, used only when fixtures has no score for a
match. So a `vb_error_absent` from `load_opta_events()` (a genuinely missing
table) is now caught locally and treated as zero events rather than as a
fatal error for the season: fixtures-backed scores still resolve normally,
and only matches with no score from *either* source still get dropped (as
before, with the existing warning). A real load failure (any other error
class) still aborts the season, as before.
## SPM coefficients now wired into the routine pipeline run (panna#173)

`export_spm_coefficients_csv()` and its `05b_export_spm_coefficients.R`
caller (`inst/extdata/spm{,_osr,_dsr}_coefficients.csv`, same `{stat_name,
beta, sd}` shape as `blend_{psr,osr,dsr}_coefficients.csv`) already existed
but were never called from `run_pipeline_opta.R` — a completely standalone
script nothing invoked. Added `step_05b_export_spm_coefficients` as a
fractional step (`start_step <= 5.5`), same idiom as the skills pipeline's
`step_08b_export_psr_weekly`, running right after step 5 (SPM) since it only
reads `cache-opta/05_spm.rds`, no refit. Not registered in a `publish_files`
accumulator — neither is the PSR/OSR/DSR equivalent; these are committed
package data under `inst/extdata`, not GitHub-Release-published pipeline
output. `check_step()` in the driver was refactored from a
`(step_num, step_name)` pair that happened to double as the `step_results`
index to a single index argument, reading the true step number/name back off
the stored result — inserting the fractional step had made those two values
diverge for every step after it, which is exactly the kind of position-based
mismatch that stays silent until a failure message prints the wrong number.

# panna 0.3.26

## The match dataset is no longer invisible from outside a pipeline run

`04_match_dataset.rds` is built on the runner and thrown away. Nothing uploads
it, so "what did the model actually see for this fixture?" has been
unanswerable without a full local run — and step 02 loads a 5.9GB cache, so
that is not a casual thing to do. Two investigations stalled on this in the
same week, both saying so in as many words: #190 ("`04_match_dataset.rds` is a
build artifact and is not available outside a pipeline run") and #192 ("the
published `predictions.parquet` does not [carry actuals], which is why this
issue stops at the symptom").

New opt-in step **4b** writes `match_features.parquet` and registers it for
`predictions-latest`: the identity columns, the team-strength features, and
the actual goals, per fixture.

* **The strength subset is defined by the same regex `07_predict_fixtures.R`
  uses** for its degraded-features guard, deliberately duplicated rather than
  reinvented. A diagnostic describing a different column set than the guard
  inspects would send the next investigation somewhere the guard never looks.
* **It aborts if that regex matches nothing**, rather than writing a valid
  empty parquet and reporting SUCCESS — the "looks like it worked" shape this
  pipeline has been bitten by repeatedly.
* Not the whole match dataset. Measured on the first published build, that is
  223 columns × 58,780 rows; the export carries 15 identity + 94 strength
  columns at ~37MB. My pre-build estimate of "~40 strength columns, a few MB"
  was wrong by roughly five times — the file is legitimately that size, and
  compression barely moves it (snappy 46.1MB → zstd 43.5MB), so the columns
  are real data rather than bloat.
* **Derived `*_diff` columns are dropped — but only the ones proved
  recomputable.** The first attempt dropped all 14 on the strength of checking
  one (`elo_diff == home_elo - away_elo`, 100% of 58,780 rows) and
  generalising. Review caught that the generalisation was false: `rest_diff` is
  `home_days_since_last - away_days_since_last`, and neither operand matches
  the keep patterns, so dropping it destroyed the only record of rest in the
  export — silently, which is the failure shape this file exists to help
  investigate.

  The step now *proves* derivability per column: for each candidate it looks
  for a surviving `(home_X, away_X)` pair whose difference reproduces it on
  real rows, and keeps anything that fails to match. No hand-maintained
  base-name map (`panna_diff` → `sum_panna` is not mechanical), and it cannot
  rot — a differential added upstream is proved or kept, never silently lost.
  Against the published build: 13 proved and dropped, `rest_diff` kept.
* **The step is non-fatal**, and the first version of it was not — a review
  finding. It sits between steps 5 and 6, so a fatal wrapper there sets
  `pipeline_failed` before steps 6, 7, 9, 10, 10b/c/d, 11–12c and 13, and
  nothing publishes at all. Position is what makes fatal expensive, not
  publish-safety. The likely trigger would have been this step's own
  abort-on-empty-regex guard, fired by a feature rename upstream: a correct
  guard taking the release down with it is the 2026-08-13 outage exactly, so
  reintroducing it the same day it was fixed would have been careless.

# panna 0.3.25 (dev)

## versebus: four silent-failure defects ported from bouncer's review (panna#187)

`R/versebus.R` was byte-identical to bouncer's pre-fix copy, so four defects
found in a bouncer code review were live here at the same line numbers. All
four turn a transient failure into a silently-accepted "everything is fine":

* **`vb_read_manifest()`'s retry-once branch classified every error as
  "confirmed absent"** instead of reusing `vb_classify_error()` like the
  first attempt does. A network blip on the retry looked identical to the
  manifest genuinely having been deleted, fell through to legacy mode, and
  **disabled sha256 verification for every download on that tag for the rest
  of the session** behind a one-time warning nobody would connect to the
  cause.
* **`vb_download()`'s `verify_by_size()` swallowed a failed asset listing
  silently.** It now warns, naming the file and saying it was accepted
  *without* verification. It deliberately still accepts it: this function is
  the sha-mismatch path's graceful degradation, and a stale manifest entry is
  far more common than real corruption, so aborting on a transient API blip
  would brick the asset until someone republished the manifest -- the exact
  outcome that caller exists to prevent. bouncer shipped the aborting version,
  its CI caught it, and it was reverted (`bouncerverse/bouncer@5edd3ac`,
  2026-08-16) for this reason. Only the silence was a defect.
* **`vb_publish()`'s cache-invalidation hook failed via bare
  `try(..., silent = TRUE)`** -- the only failure path in this file with no
  logging at all. A dead hook meant downstream consumers kept serving
  pre-publish data indefinitely with nothing recording why.
* **`vb_generation()` ran `max()` on `updated_at` with no `na.rm`.** One
  unrelated asset missing a timestamp (which `vb_list_assets()` deliberately
  tolerates as `NA` rather than failing the whole listing) silently turned
  the entire generation into `NA`, indistinguishable from "no assets at
  all". Latent today (no caller in this package yet).

All four fixed in place per bouncer's `peteowen1/bouncer@86e2ebc`; each has a
dedicated regression test in `tests/testthat/test-versebus.R`, mutation-tested
by reverting the fix and confirming the test fails. `torpverse/torp/R/versebus.R`
carries the same four defects (confirmed, not yet fixed) -- see panna#187.
## Step 12 now checks step 11 finished, not just that its files exist

Review of the change below found the existence check was not enough, and the
gap is not theoretical. Step 11 writes `wc2026_bt_ratings.parquet` **before**
`simulate_world_cup()` runs and `wc2026_simulation.parquet` /
`wc2026_group_expectations.parquet` **after**. So a failure inside the
simulation — a documented, previously-observed one — leaves a *fresh* BT file
beside *stale* simulation files, with all three present. `file.exists()` on
the three passes, section 5 merges the two vintages into one
`wc2026_team_strength.parquet`, and step 13 publishes it. Step 11 has been
non-fatal since panna#194, so "failed partway and carried on" is now routine
rather than rare.

* **Step 11 writes a commit marker.** It deletes `.wc11_outputs_complete` on
  entry and rewrites it only after all three outputs are on disk. Step 12
  requires it, so a partial run reads as "not ready" rather than "ready".
* **`build_id` is only stamped on files the run actually wrote.**
  `.vb_generation_stamp()` is wall-clock plus run id — a per-run stamp, not a
  data vintage — so stamping a file the run did not rewrite asserts a
  freshness that isn't there, and hands two genuinely different vintages the
  same id. That is precisely the signal the blog's `detectMixedBuild()` reads
  to spot a torn publish. Skipped outputs now keep the `build_id` they were
  last published with, which is the honest answer: they *are* from an earlier
  run. Post-tournament this means the World Cup pages will show a "mixed data
  build" label, correctly — the predictions refresh while the simulation stays
  frozen at the final.

## Step 12's blog export ran into the WC2026 liveness gate it should never have been in

panna#194's WC2026 liveness gate (0.3.23 below) disabled steps 11/12/12b/12c
together once the tournament was over. Step 12 should not have been on that
list: it *exports*, it does not simulate. Its match-prediction exports
(`wc2026_predictions.parquet`, `wc_history_predictions.parquet`) read only
`07_predictions.rds` and are exactly as valid post-tournament as pre- — they
are how the blog browses a *finished* World Cup — but with step 12 gated off,
both files stayed frozen at their 2026-08-12 publish (0 rows / 140-byte
header-only CSV) even after the separate #191 fix restored WC2026 rows to
`07_predictions.rds`.

* `run_predictions_opta.R` no longer includes `step_12_export_wc2026_blog` in
  `.wc_steps` — step 12 now runs on every predictions-pipeline run,
  regardless of tournament liveness. Steps 11, 12b and 12c stay gated (they
  simulate, or snapshot a simulation in progress).
* `12_export_wc2026_blog.R` computes `.wc11_available` from whether step 11's
  three output files exist (`wc2026_simulation.parquet`,
  `wc2026_group_expectations.parquet`, `wc2026_bt_ratings.parquet`) and gates
  the sections that read them (3, 4, 5, 5c, and the reference-fact validation
  in 8) on that flag instead of hard-erroring via a bare `read_parquet()`.
  Sections 2/2b (match predictions) and the existing build_id-stamp/publish-
  registration logic in 6/7 are unaffected — 6/7 already tolerated absent
  files file-by-file. The four step-11-dependent sections are gated together
  as one block, not independently, because section 5's team-strength numbers
  and section 5c's per-player squad ratings are two views of one
  "team == Sigma(squad)" invariant the blog relies on; refreshing one without
  the other would break it.
* Also fixed while adding a Windows-run test for this: the build_id-stamp
  loop in section 6 read and immediately overwrote the same parquet path
  without `mmap = FALSE`, which fails with a file-locking error on Windows
  (section 3 already used `mmap = FALSE` for exactly this read-then-rewrite
  pattern).
* `tests/testthat/test-wc2026-export.R` sources the real step-12 script
  against a fixture cache holding only `07_predictions.rds` and proves the
  match-prediction exports are written and the step-11-dependent ones are
  not, including the case where only one of the three step-11 files is
  present (must still count as unavailable). Mutation-tested: forcing
  `.wc11_available` to always `TRUE` reproduces the pre-fix hard error.

# panna 0.3.24

## A double quote in a workflow comment truncated the R payload

The first run on `main` after the fix below died two minutes in with
`Error: unexpected end of input`. A comment added to
`predictions-pipeline.yml` read `# so a log line reading "DISABLING steps
11/12/12b/12c"`. Workflows run R via `Rscript -e "<payload>"`, and the shell
ends that string at the first unescaped `"` — so the payload the shell handed
R was **74 lines instead of 97**, cut off mid-comment. Harmless in a `.R`
file; fatal here.

YAML validation passed, every `.R` file parsed, and the review missed it,
because nothing checked the string the *shell* would build.
`tests/testthat/test-workflow-r-payloads.R` now reconstructs each
`Rscript -e` payload the way the shell does and parses it. Mutation-tested:
reintroducing the quote reproduces the production error verbatim.

# panna 0.3.23

## The World Cup branch no longer takes the daily publish down with it

The Predictions Pipeline failed every day from 2026-08-13 to 2026-08-21 and
published nothing for eight days. The predictions themselves were fine and sat
in the cache the whole time.

* **Steps 11/12/12b/12c are gated on the tournament still being live.** They
  simulate a World Cup in progress. After the final, step 11 found zero WC2026
  rows and carried on anyway — fitting Bradley-Terry ratings and reporting
  champion probabilities for a competition already won, reporting SUCCESS.
  Then `build_knockout_lookup()` gained a correct invariant (a team's
  aggregates must be constant across its own WC rows, since the lookup reads
  row `[1]`) which holds for unplayed fixtures carrying one as-of snapshot and
  fails for played rows carrying per-match ones — so from the next run it
  aborted on Argentina. `run_predictions_opta.R` now asks
  `.wc2026_fixtures_remaining()` first and turns the four steps off, loudly,
  when nothing is left to play. `11_simulate_wc2026.R` refuses to start on the
  same condition for standalone runs, naming the real cause instead of
  Argentina's feature values.
* **`.wc2026_fixtures_remaining()` answers "cannot tell" rather than guessing.**
  A missing cache, a missing column, and WC rows whose `status` is entirely NA
  all return `NA_integer_`, which leaves the steps as configured. Only a real
  count of zero disables them — `sum(na.rm = TRUE)` over a corrupt column would
  have reported 0, i.e. "tournament over", and turned the branch off with a
  tidy log and no symptom.
* **Unresolved knockout slots do not count as fixtures remaining.** They sit in
  the data as blank-team placeholders with status `"fixture"`, and
  `11_simulate_wc2026.R` drops them from the set it simulates. Counting them in
  the gate would have left the WC steps enabled on a day step 11 had nothing to
  run. Step 11's standalone guard now calls the same helper instead of
  recomputing its own count, so the two cannot drift apart again.
* **A World Cup failure is no longer fatal to the pipeline.**
  `run_pipeline_step()` sets `pipeline_failed` on any failure, which makes
  every later step print "SKIPPED (previous step failed)" — including step 13,
  the one and only publish. That is right for the model chain and wrong for a
  side branch nothing else consumes. Steps 11/12/12b/12c now go through
  `run_pred_step_optional()`: still FAILED in the summary, still in the log,
  but the publish proceeds. Steps 09/10/10b/10c/10d keep the fatal treatment,
  because they register files in `publish_files` and half of one failing is a
  real reason to hold the release back.
* **Non-fatal is not the same as invisible.** The workflow's exit code follows
  `pipeline_failed`, so a WC step failing for a real reason — a bug, not "the
  tournament ended" — would leave a green tick and a job summary still claiming
  blog data was uploaded. Each non-fatal failure is now recorded to a marker
  file that the workflow's summary step turns into a GitHub warning annotation.
  The run stays green on purpose; it just stops being silent.


## The 13 "flaky" test failures were one bug, hiding two more

The suite had 13 failures across four files that each appeared to pass in
isolation, so they read as flaky and had been left alone. They were neither
flaky nor independent.

* **A test was reloading the package mid-suite.** `test-publish-gating.R`
  `source()`s real pipeline scripts, and every numbered step opens with
  `devtools::load_all()` so it can run standalone. Under `devtools::test()` that
  reloads the namespace mid-run, which (a) discards the
  `local_mocked_bindings()` the sourcing test just installed — so `vb_publish`
  un-mocks itself partway through and the test errors on its own — and (b)
  swaps the package's session-state environments (`.opta_env`,
  `.opta_remote_env`, `.vb_state`) for fresh ones, breaking every file that runs
  after it. Confirmed by address: `.opta_remote_env` is a different environment
  before and after. The package is already loaded when testthat runs, so the
  script's own `load_all()` is redundant there; it is stubbed for the duration
  of those four tests. **That alone fixed 11 of the 13.**
* **`09_export_ratings.R` has published four files since panna#165** — the raw
  RAPM pair must advance together with the shrunk pair — but the fixture still
  supplied two, so the script aborted on `seasonal_rapm is empty or NULL`. The
  M-RATINGS-PAIR invariant test had not been checking the current contract.
* **The both-or-neither test asserted only an error _class_**, so it would have
  passed against any early abort — vacuous the moment a fixture went stale,
  which is exactly what had happened. It now asserts it reached `vb_publish`.
* The fixture's synthetic `replacement` row exercised the export-boundary drop
  without asserting it; row counts are now captured inside the mock (the script
  unlinks its temp dir as soon as `vb_publish` returns) so the assertion fails
  if the drop regresses.
* Closed a second, non-failing leak: the two `download_opta_catalog` tests
  cleared the session cache on the way in but not out, leaving a one-league
  fixture catalog behind for later readers. A residual path still populates it
  indirectly from elsewhere in that file; it causes no failures today.

**723 tests, 0 failures.** Bisected in the real `devtools::test()` harness —
sequential `test_file()` does not reproduce it, which is part of why it
survived.

# panna 0.3.22 (dev)

## Pipeline guards: partial loads and skipped steps now fail instead of reporting success

Both of these were cases where the pipeline finished, printed SUCCESS, and
published incomplete data. Neither was detectable from the logs.

* **`01_load_opta_data.R` now asserts league-season coverage.** The per-season
  error handler logs and continues (deliberate — one bad league-season must not
  kill a 40-minute run), but nothing counted the losses: ~49 league-seasons died
  on `non-character argument` in the 2026-06 rerun and the run still reported
  success. The two end-of-step guards could not catch it — the league check
  passes when a single season per league survives, and the 100-match floor is
  three orders of magnitude below a real run. Outcomes are now counted
  separately: an **exception** is never legitimate and fails the step
  (`max_failed_league_seasons`, default 0), while **missing upstream data**
  (no lineups / no events / no SPADL) is listed and held to a coverage floor
  (`min_coverage_frac`, default 0.90). Both are overridable before sourcing.
  * Coverage is measured on the **shots/xG** grain, not lineups. Lineups are
    staged before the raw-events and SPADL checks, so a season whose events
    failed still landed in `combined_lineups` and looked covered while carrying
    no xG — and that is reachable for a whole league at once, since
    `load_opta_match_events()` is a per-league call.
  * **League-level losses are tracked too.** The season ledgers start inside the
    season loop, so a league dropped at the `list_opta_seasons()` step was in
    none of them — coverage computed 100% over the leagues that did run while a
    whole competition was missing. A listing *exception* now aborts (it was
    being swallowed into `character(0)`, making a corrupt catalog
    indistinguishable from "not scraped yet"); a league that legitimately lists
    zero seasons is reported in the summary.
* **`run_step()` aborts on a step key that isn't in `run_steps`.** It previously
  returned `DISABLED` — visually identical in the summary to a deliberate
  `FALSE`. Renaming a step at one of its two call sites, or a GHA `run_steps`
  override drifting after a rename, silently turned the step into a no-op.
  `print_pipeline_summary()` also now **aborts** on `run_steps` keys that no
  call site consumed (the reverse drift: a typo'd override). It warned at first,
  on the reasoning that failing after the work is done is pointless — but a key
  nothing consumes means the step it names never ran, so the output *is*
  incomplete, and `warning()` sets no exit code in `Rscript`, leaving CI green.
  Set `allow_unconsumed_step_keys <- TRUE` for a deliberately partial run. The
  guard found two already-stale local debug runners on its first run.
* `xg_model.R`: keeper GSAA now warns loudly whenever the GK columns don't land,
  instead of silently dropping `gsaa`/`gsaa_per90`/`xgot_faced`/`goals_conceded`
  from the output — the GK PSR/PSV sub-model is built on `gsaa_per90`, so losing
  it scored every keeper with no shot-stopping credit. The warning is gated on
  "the join did not happen", **not** on an exception being thrown:
  `.compute_keeper_gsaa()` has six ordinary `return(NULL)` early exits (missing
  columns, no shots, no keepers with minutes, …), and warning only in the
  `tryCatch` handler covered the rare path while leaving every common one
  silent. Keepers missing from the GSAA join are left `NA` rather than filled
  with 0; 0 is correct for an outfielder but fabricates "exactly league-average
  shot-stopping" for a keeper.
* `load_opta_eventless_ids()` distinguishes a registry that doesn't exist yet
  (quiet, normal) from one that exists and won't parse or has lost its
  `match_id` column (warns). Both still return empty, but the second case is a
  corrupt download or upstream schema change and was indistinguishable before —
  and since the registry is *subtracted* from the coverage denominator, silently
  empty turns every legitimately event-less match into an apparent gap.

## `extract_season_end_year()` is vectorized

* It was scalar-only: the `is.na(season) || !nzchar(season)` guard is a hard
  error on length > 1 under R >= 4.3, so every call site had to remember a
  `vapply()` wrapper, and one that didn't was a crash rather than a wrong
  number — `.season_end_year_for_date()` passed a vectorized
  `extract_season_from_date()` result straight through. Scalar behaviour is
  unchanged (verified identical across 25 inputs including all three season
  label formats), so existing `vapply()` call sites keep working.
* `07_seasonal_ratings.R` / `01_load_opta_data.R`: replaced
  `sapply(unique(x), extract_season_end_year)` with a `setNames()` lookup —
  `sapply` returns a *list* on empty input — and switched the season-matching
  index to `which()`, since an unparseable label yields `NA` and `names(v)[NA]`
  injects an `NA` that then matches every `NA`-season row.

## `opta_loaders.R` split into four files

* At 2,635 lines it held four unrelated responsibilities. Now:
  `opta_paths.R` (data directory, league-code translation, season discovery,
  SQL/parquet helpers), `opta_coverage.R` (event-less registry + coverage gate),
  `opta_remote.R` (catalog download/TTL cache, league listing, remote query
  engine), and `opta_loaders.R` (the `load_opta_*()` family, 1,313 lines).
* Pure move, mechanically verified: all 563 namespace objects present with
  byte-identical function bodies before and after, `NAMESPACE` unchanged, and
  the only `man/` churn is the `% Please edit documentation in R/...` line.

## Other

* `versebus.R` (synced with canonical `torpverse/torp`, now `VERSEBUS_VERSION`
  1.1.0): `vb_publish()` restores `piggyback_cache_duration` on exit instead of
  leaking it for the rest of the session; `.vb_generation_stamp()` no longer
  calls `sample()`, which advanced the caller's RNG stream and silently changed
  the draws of any simulation seeded before a publish.
* `spm_panel.R`: per-90 rate columns are written with `data.table::set()` rather
  than `[[<-` (which forces a full copy), and zero-minute rows get `NA` rather
  than a fabricated 0. The `NA` still becomes 0 downstream via
  `.clean_numeric_na()`, but now goes through that function's counter, so the
  imputation is reported instead of invisible.
* `01_load_opta_data.R`: `events`/`stats` are guarded before use. When their
  league-level load failed they were `NULL`, and `events$league <- league`
  coerces `NULL` into a *list*; the season was dropped only because
  `.stage_write()` then threw on a zero-length condition, at a misleading call
  site.

# panna 0.3.21 (dev)

## Skills join fixed — PSR/PSV were trained on half their weekly bins

* **`07_train_psr_model.R`: the chunked skill join visited every second weekly
  date.** `prematch_skills[[j]] <- NULL` deletes a list element rather than
  blanking it, so the list shrank under the loop while `names(prematch_skills)[j]`
  re-read it; past the halfway point the lookup returned `NULL` and an
  `is.null()` guard swallowed it. 338 of 677 bins were skipped, their
  player-matches re-added with `NA` skills and **imputed to 0**, then
  minute-weighted into team totals — silently, with the loop reporting success.
  Live since `a317281` (2026-03-17). Coverage is now 100.0%
  (1,885,239 / 1,885,715 player-matches). **All PSR/PSV/blend/goal-diff
  coefficients are regenerated.** The GK path was never affected (it iterates
  `names()` directly) and its coefficients are byte-identical.
* The damage was **not** simple attenuation: zero-filling half the rows changed
  which features the penalized fit *selected* (PSR exact-zero betas 43 → 10,
  OSR 40 → 49). Anything derived from the old coefficients must be refit, not
  rescaled.
* **New tripwires**: abort if >5% of weekly dates go unjoined, or if <95% of
  player-matches have skills. Both numbers were already printed; nothing
  asserted on them.
* Orphaned player-matches are now found by an anti-join on the
  `(match_id, player_id)` **pair** rather than an OR of two independent
  membership tests, which could only see a wholesale-absent match or player.
* `PSV_RELIABILITY_GD_SCALE` re-derived, 5.134 → **2.717**, and
  `psv_live_constants.csv` rebuilt. The old value had been stale since
  2026-07-21: two retrains changed the coefficients it is fit against and
  neither re-derived it. Step 07 now names the artifacts it invalidates, and
  07d warns when its fitted slope drifts >2% from the shipped constant.

## Correctness guards in the RAPM core

* `validate_parquet_file()` returns `TRUE`/`FALSE`/**`NA`**, and `TRUE && NA` is
  `NA`, so `if (NA)` **aborted** at three cache-read sites instead of falling
  through to the re-download they were written for. Now `isTRUE`/`isFALSE`; the
  `NA` case refetches without deleting a file it could not validate.
* The penalty factor is built positionally, assuming covariates occupy the last
  columns — now asserted rather than assumed. `covariate_names`/`covariate_cols`
  unified across `fit_rapm()` and `fit_rapm_with_prior()`.
* `fit_rapm_with_prior()` gains `parallel`/`n_cores` (the production xRAPM path
  was the serial one), aborts on a non-finite prior, and reports a weighted R²
  denominator to match its weighted `cvm`.
* All five `player_mapping` name-joins share one duplicate-guarded helper; a
  duplicated `player_id` previously multiplied rating rows silently.
* `.subset_rapm_data_expanding()` rejects a net-mode design instead of returning
  a vector of `NA`s.

# panna 0.3.20 (dev)

## Simplification campaign — dead code removal, vignette rewrite, API curation

* **Opta-only cleanup**: removed the dead FBref/Understat processing chain and
  the slow FBref splint path, the legacy piggyback tarball download layer,
  deprecated EPV stubs, a duplicate `pb_download_opta()`, and other dead
  `utils.R` helpers. `globals.R` pruned of stale/duplicate entries.
* **`panna_rating.R` (the RAPM+SPM glmnet blend) removed** — superseded by the
  EPR/PSR/Panna/Piero composite system; no longer referenced anywhere in the
  pipeline.
* **Vignettes rewritten for the current system**: `getting-started`,
  `player-ratings` (EPR/PSR/Panna/Piero), and `pipeline-walkthrough` (now a
  "Pipeline Anatomy" reference) rewritten; two new vignettes added,
  `match-prediction` (prediction reading + WC2026 tournament simulation) and
  `data-bus` (downloading and publishing pipeline data). README refreshed to
  match, dead-function examples removed.
* **`data-raw/` extraction**: shared pipeline helpers (banner, step-runner,
  blog leagues, backfill) factored into `pipeline_utils.R`.
* **Reference index curation**: removed 30 phantom entries from
  `_pkgdown.yml` (non-exported functions that still had a listed topic,
  including the by-then-deleted `fit_panna_model` family) and the two
  sections that emptied out as a result. Added `@family` tags to every
  exported function mirroring the reference index's section structure, then
  converted each section's `contents:` to `has_concept("<family>")` so the
  index self-heals instead of drifting from `NAMESPACE`. Fixed six roxygen
  defects that were producing `R CMD check` warnings (missing titles,
  undocumented arguments, a malformed `\link{}` cross-reference, a merged
  roxygen block that had bled one function's docs onto its neighbour).
* **Conservative export prune**: un-exported 11 of 198 functions
  (`apply_canonical_player_ids`, `augment_ratings_with_history`,
  `build_minutes_training_data`, `build_player_id_canonical_map`,
  `clean_column_names`, `fit_minutes_model`, `fit_spm_opta_target`,
  `optimize_epr_decay`, `save_xg_model`, `save_xpass_model`,
  `weight_rating_by_minutes`) — each had zero callers outside `R/` and is
  internal-shaped plumbing (a builder, fitter, or low-level converter); the
  console-facing API (player lookups, Opta loaders, the data bus,
  prediction/simulation entry points, constants) is untouched.
* **DESCRIPTION** Title/Description rewritten for the current Opta-only,
  RAPM+SPM+EPV+WPA-based rating system (previously still described FBref/
  Understat as active sources).

## `panna_value` → `piero_value` rename (2026-07-06, not previously recorded)

* Renamed the `panna_value`/`panna_value_p90` columns produced by
  `build_player_game_ratings()` and `player_value()` to `piero_value`/
  `piero_value_p90` (column names only — the underlying 50/50 EPV+PSV
  per-match blend is unchanged). The old name implied "per-match panna",
  which the metric never was — single-game RAPM is too noisy for panna to
  have a per-match twin. `piero_value` is Piero's value counterpart through
  the R↔V pairing (EPR↔EPV, PSR↔PSV).

## EPR rebuild — regression-based + league/opponent FE

* **`calculate_epr_regression()`** in `player_ratings_epv.R` — new weighted ridge regression for EPR:
  `epv_p90 ~ β_player + α_league_season + γ × opp_def_rating + ε` with `exp(-Δt/decay) × mins/90` weights. Player coefficients are L2-penalized (Bayesian shrinkage), league-season and opponent terms are unpenalized fixed effects. Returns β_player as the new EPR.
* **`optimize_epr_decay()`** — held-out MSE grid search for the decay parameter. Empirical winner: 900 days. All candidates within 0.001% of each other, so decay turns out to be near-irrelevant.
* **opp_def_rating** sourced from `cache-opta/team_season_strength.parquet` (minute-weighted aggregation of player panna/offense/defense per team-season). Drives most of the league-bias correction.
* WC sim impact: Türkiye dropped #2 → #5 on team mw_epr; Croatia #4 → #12; Morocco #9 → #13. Top-20 individual EPR (Mbappé, Kane, Veerman, Tavernier, Kimmich, Olise, Yamal, etc.) is now uniformly elite players.

## aggregate_lineup_ratings — silent value-metric drop fix

* **Bug**: the `rating_cols` filter in `R/match_prediction.R` only kept `panna/offense/defense/spm` from input ratings, silently dropping any value-metric columns (`psr/osr/dsr`, `epr/epr_offensive/epr_defensive`, `wpa_rating`, `psv_rating`, `centrality`) before the function's own `has_psr`/`has_epr` checks could see them. Result: `sum_psr`/`sum_epr`/etc. were never created in team-level features.
* **Fix**: `rating_cols` now uses `intersect(known_optional, names(dt_ratings))` to carry through any present value-metric columns. Coverage-shrunk team-mean imputation extended to those columns so missing-data teams (Mexico/Korea/Canada/USA) aren't biased low on sum_epr.
* **Diagnostic**: opt-in warning when expected optional columns are missing — set `options(panna.verbose_ratings = TRUE)`.

## PSR — league-season FE in skill→xG regression

* `07_train_psr_model.R` adds unpenalized league-season FE columns to the team-aggregated skill regression. PSR betas are now estimated controlling for league baseline, making cross-league rankings comparable. Türkiye correctly drops to mid-pack on PSR.
* Tried team-season FE first (over-corrected — team strength is partly caused by player skill, stripped elite players of their contribution to their teams). League-season FE is the right granularity.

## Pipeline robustness

* Data.table NSE fixes in `data-raw/match-predictions-opta/05_fit_goals_model.R`, `06_fit_outcome_model.R`, `07_predict_fixtures.R`, `08_evaluate_model.R` — wrapped filtered subsets in `as.data.frame()` so `train_data[, feature_cols, drop = FALSE]` works when input is a data.table.
* Brazilian Serie A EPR coverage extension via standalone `debug/keep/build_bra_game_logs.R` (Brazilian seasons use single-year format that 10b_export_game_logs.R can't handle directly). Brazil EPR coverage 84.6% → 96.2% post-fix.

# panna 0.3.0

Value metrics infrastructure, pipeline hardening, and match prediction improvements.

## Value Metrics (Two-Path System)

* **EPR** (Expected Points Rating) — Decay-weighted Bayesian EPV ratings per player. New functions: `calculate_epr()`, `calculate_epr_batch()` in `player_ratings_epv.R`
* **WPA** (Win Probability Added) — 3-class win probability model (H/D/A) with action-level WPA credit assignment. New functions: `create_wp_features()`, `train_wp_model()`, `add_wp_vars()`, `assign_wpa_credit()`, `aggregate_player_game_wpa()`
* **PSV/PSR** (Player Stat Value / Player Skill Rating) — Per-game stat value via glmnet coefficients with O/D decomposition. New functions: `calculate_psv()`, `calculate_psv_components()`, `calculate_psr()`
* **panna_value** — Combined per-game metric: 50% EPV + 50% PSV. `build_player_game_ratings()` merges EPV + WPA + PSV into unified per-game output
* **Multi-target RAPM** — xG, EPV, WPA, PSV as response variables via `fit_spm_opta_target()`

## New Features

* **Player centrality** — PageRank-based network centrality from opponent graphs. `calculate_player_centrality()` in `centrality.R`, integrated as step 07b in Opta pipeline
* **Player attribution** — Zero-ablation contribution method. `calculate_player_attribution()` + `batch_player_attribution()` in `player_attribution.R`
* **Weather integration** — Weather data for match features via `weather.R`
* **Match simulation** — Monte Carlo season simulation engine via `simulate.R`
* **Player comparison** — Side-by-side player comparison via `compare_players.R`

## Pipeline Improvements

* **Unified `run_step()`** in `pipeline_utils.R` — serves all 4 pipelines (Opta, FBref, Skills, Predictions)
* **Skills pipeline expanded** to 12 scripts (00, 01-06, 07, 08, 08b) with PSR model training and weekly PSR exports
* **EPV pipeline expanded** to 6 steps — WP model training (step 05) and WPA calculation (step 06) added
* **Opta pipeline GHA** — `opta-pipeline.yml` for running Opta RAPM/SPM on GitHub Actions with auto cache upload
* **Bootstrap script** — `data-raw/bootstrap.R` for one-command fresh clone setup (data + models + caches)
* **Predictions pipeline** — Skills-based team features (step 02b), blog dispatch guard (`if: success()`)

## Bug Fixes & Hardening

* Fixed `library()` vs `requireNamespace()` in PSOCK parallel workers — workers need `library()` to attach packages
* Fixed `on.exit()` calls to use `add = TRUE` to prevent DuckDB connection leaks
* Pipeline joins migrated from `player_name` to `player_id` (one legacy join remains in `06_xrapm.R`)
* `filter_bad_xg_data` threshold set to 30% for Opta (25% zero-xG splints is normal with SPADL)

## Tests

* 35 test files, 2248+ expectations across loaders, models, pipelines, scraping, and value metrics

# panna 0.2.0

Major expansion: Opta is now the primary data source with full pipeline support across 15 leagues.

## Breaking Changes

* `load_opta_*()` functions now default to `source = "remote"` instead of `"local"`. Existing code that relied on the default loading from local files will now download from GitHub. Add `source = "local"` explicitly to restore the old behavior. (#11)
* `player_skill_profile()` now errors (with name suggestions) when a player is not found, instead of returning `NULL` with a warning. Code checking `is.null(result)` should use `tryCatch()` instead. (#13)
* `player_skill_profile()` return columns changed: `weighted_90s` and `confidence` removed; new columns `type`, `raw_avg`, `n90`, `w90`, `attempts`, `w_attempts` added. (#13)
* `to_opta_league()` now errors on unknown league codes when the catalog is available (previously warned and passed through). Typos like `"EPLL"` now fail fast. (#11)
* `suggest_opta_seasons()` is no longer exported (now internal). Use `list_opta_seasons()` instead. (#11)

## User Experience Improvements

* `load_opta_*()` functions now default to `source = "remote"`, so data loads directly from GitHub without requiring `pb_download_opta()` first (#11)
* `to_opta_league()` now accepts case-insensitive input: "epl", "eng", "Eng" all work (#11)
* `list_opta_seasons()` and `list_opta_leagues()` now accept `source = "remote"` as an alias for `"catalog"` for consistency with `load_opta_*()` functions (#11)
* Local-only error messages now suggest `source = 'remote'` as an alternative (#11)
* `player_skill_profile()` auto-loads pre-computed skills and match stats from GitHub releases when called with no data, instead of downloading ~200 MB of raw stats (#13)
* New `load_opta_skills()` function for loading pre-computed skill estimates from GitHub releases (#13)
* Corrupt parquet files from interrupted downloads are now detected and re-downloaded automatically (#12)
* DuckDB "No magic bytes" errors now give a clear message and auto-remove the corrupt cache (#12)
* Fixed `on.exit()` calls to use `add = TRUE` to prevent DuckDB connection leaks (#12)

## Opta RAPM/SPM Pipeline

* Full RAPM/SPM/Panna rating pipeline for Opta data (15 leagues, 42K+ matches)
* Opta SPM uses 80+ features including xMetrics enrichment
* Parallel pipeline scripts in `data-raw/player-ratings-opta/`
* 15 leagues: Big 5 + NED/POR/TUR/ENG2/SCO + UCL/UEL/UECL + WC/EURO

## EPV (Expected Possession Value)

* Action-level player valuation from Opta event data with x/y coordinates
* SPADL conversion for Opta events (`convert_opta_to_spadl()`)
* XGBoost models for xG, xPass, P(scoring), P(conceding)
* EPV credit assignment with pass credit splitting
* Pre-trained models stored in GitHub Releases

## xMetrics Pipeline

* Pre-computed xG/xA/xPass metrics for all 15 Opta leagues
* Uses pre-trained SPADL + XGBoost models (penalty xG overridden to 0.76)
* Output as parquet files per league/season
* Loaded via `load_opta_xmetrics()`

## Estimated Skills

* Bayesian decay-weighted skill estimation with exponential recency weighting
* Position-specific prior multipliers (GK/DEF/MID/FWD)
* 3-pass optimization of prior strength, lambda, and quantile
* Context adjustments for opponent quality, venue, and league level
* Player skill profiles with percentiles and confidence intervals
* Backtest framework for evaluating prediction accuracy
* New functions: `estimate_player_skills()`, `player_skill_profile()`, `backtest_skill_predictions()`, `optimize_all_priors()`

## Match Predictions

* XGBoost Poisson model for home/away goal counts
* XGBoost multinomial model for W/D/L probabilities
* Elo rating system with `init_team_elos()`, `compute_match_elos()`
* Rolling team form features (5/10/20 match windows)
* Team-level skill feature aggregation from estimated skills
* Calibration and logloss evaluation tools

## New Opta Data Loaders

* `load_opta_match_events()` - All events with x/y coordinates
* `load_opta_lineups()` - Lineup data
* `load_opta_fixtures()` - Fixture/results data with match status filtering
* `load_opta_xmetrics()` - Pre-computed xG/xA/xPass metrics
* `load_opta_shot_events()` - Individual shots with coordinates
* `load_opta_events()` - Goals, cards, substitutions
* `list_opta_leagues()` - Automatic league discovery from data catalog
* `load_opta_match_stats()` - Load pre-computed match-level statistics

## Other Improvements

* Opta column count corrected to 263 (was incorrectly documented as 271)
* Opta xG model documented as SPADL + XGBoost (was incorrectly shown as "None")
* Opta history starts 2013+ (was incorrectly shown as 2010+)

# panna 0.1.0

Initial release of the panna player rating system.

## Rating System

* RAPM (Regularized Adjusted Plus-Minus) implementation using splint-based analysis
* SPM (Statistical Plus-Minus) for box score prediction of RAPM
* Combined Panna ratings with SPM as Bayesian prior
* Offensive and defensive rating decomposition

## Data Sources

* FBref support with StatsBomb xG (Big 5 leagues, cups, international)
* Opta support with 263 columns per player (15 leagues, 2013+)
* Understat support with xGChain and xGBuildup (Big 5 + Russia)

## Data Loading

* DuckDB-based efficient parquet loading
* `load_summary()`, `load_passing()`, `load_defense()`, `load_possession()`, `load_shots()`, `load_metadata()` for FBref
* `load_opta_stats()`, `load_opta_shots()`, `load_opta_big5()` for Opta
* `load_understat_roster()`, `load_understat_shots()` for Understat

## Player Statistics

* Aggregated player statistics with `player_fbref_*()`, `player_opta_*()`, `player_understat_*()` functions
* Filtering by minimum minutes, leagues, and seasons

## Data Distribution

* GitHub Releases integration via piggyback
* `pb_download_source()` for data download
* `pb_upload_parquet()` for data upload
