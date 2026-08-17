# panna 0.3.21 (dev)

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
* **`run_step()` aborts on a step key that isn't in `run_steps`.** It previously
  returned `DISABLED` — visually identical in the summary to a deliberate
  `FALSE`. Renaming a step at one of its two call sites, or a GHA `run_steps`
  override drifting after a rename, silently turned the step into a no-op.
  `print_pipeline_summary()` also now reports `run_steps` keys that no call site
  consumed (the reverse drift: a typo'd override), as a warning rather than an
  abort, since by summary time the work is already done. The guard found two
  already-stale local debug runners on its first run.
* `xg_model.R`: a failed keeper-GSAA computation now warns loudly instead of
  silently dropping `gsaa`/`gsaa_per90`/`xgot_faced`/`goals_conceded` from the
  output — the GK PSR/PSV sub-model is built on `gsaa_per90`, so losing it
  scored every keeper with no shot-stopping credit. Keepers missing from the
  GSAA join are left `NA` rather than filled with 0; 0 is correct for an
  outfielder but fabricates "exactly league-average shot-stopping" for a keeper.
* `load_opta_eventless_ids()` distinguishes a registry that doesn't exist yet
  (quiet, normal) from one that exists and won't parse or has lost its
  `match_id` column (warns). Both still return empty, but the second case is a
  corrupt download or upstream schema change and was indistinguishable before —
  and since the registry is *subtracted* from the coverage denominator, silently
  empty turns every legitimately event-less match into an apparent gap.

## `extract_season_end_year()` is vectorized

* It was scalar-only: the `is.na(season) || !nzchar(season)` guard is a hard
  error on length > 1 under R >= 4.3, so all ~40 call sites had to remember a
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
