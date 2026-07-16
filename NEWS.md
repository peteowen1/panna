# panna 0.3.1 (dev)

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
