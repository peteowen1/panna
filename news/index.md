# Changelog

## panna 0.3.0

Value metrics infrastructure, pipeline hardening, and match prediction
improvements.

### Value Metrics (Two-Path System)

- **EPR** (Expected Points Rating) — Decay-weighted Bayesian EPV ratings
  per player. New functions:
  [`calculate_epr()`](https://peteowen1.github.io/panna/reference/calculate_epr.md),
  [`calculate_epr_batch()`](https://peteowen1.github.io/panna/reference/calculate_epr_batch.md)
  in `player_ratings_epv.R`
- **WPA** (Win Probability Added) — 3-class win probability model
  (H/D/A) with action-level WPA credit assignment. New functions:
  [`create_wp_features()`](https://peteowen1.github.io/panna/reference/create_wp_features.md),
  [`train_wp_model()`](https://peteowen1.github.io/panna/reference/train_wp_model.md),
  [`add_wp_vars()`](https://peteowen1.github.io/panna/reference/add_wp_vars.md),
  [`assign_wpa_credit()`](https://peteowen1.github.io/panna/reference/assign_wpa_credit.md),
  [`aggregate_player_game_wpa()`](https://peteowen1.github.io/panna/reference/aggregate_player_game_wpa.md)
- **PSV/PSR** (Player Stat Value / Player Skill Rating) — Per-game stat
  value via glmnet coefficients with O/D decomposition. New functions:
  [`calculate_psv()`](https://peteowen1.github.io/panna/reference/calculate_psv.md),
  [`calculate_psv_components()`](https://peteowen1.github.io/panna/reference/calculate_psv_components.md),
  [`calculate_psr()`](https://peteowen1.github.io/panna/reference/calculate_psr.md)
- **panna_value** — Combined per-game metric: 50% EPV + 50% PSV.
  [`build_player_game_ratings()`](https://peteowen1.github.io/panna/reference/build_player_game_ratings.md)
  merges EPV + WPA + PSV into unified per-game output
- **Multi-target RAPM** — xG, EPV, WPA, PSV as response variables via
  [`fit_spm_opta_target()`](https://peteowen1.github.io/panna/reference/fit_spm_opta_target.md)

### New Features

- **Player centrality** — PageRank-based network centrality from
  opponent graphs.
  [`calculate_player_centrality()`](https://peteowen1.github.io/panna/reference/calculate_player_centrality.md)
  in `centrality.R`, integrated as step 07b in Opta pipeline
- **Player attribution** — Zero-ablation contribution method.
  [`calculate_player_attribution()`](https://peteowen1.github.io/panna/reference/calculate_player_attribution.md) +
  [`batch_player_attribution()`](https://peteowen1.github.io/panna/reference/batch_player_attribution.md)
  in `player_attribution.R`
- **Weather integration** — Weather data for match features via
  `weather.R`
- **Match simulation** — Monte Carlo season simulation engine via
  `simulate.R`
- **Player comparison** — Side-by-side player comparison via
  `compare_players.R`

### Pipeline Improvements

- **Unified `run_step()`** in `pipeline_utils.R` — serves all 4
  pipelines (Opta, FBref, Skills, Predictions)
- **Skills pipeline expanded** to 12 scripts (00, 01-06, 07, 08, 08b)
  with PSR model training and weekly PSR exports
- **EPV pipeline expanded** to 6 steps — WP model training (step 05) and
  WPA calculation (step 06) added
- **Opta pipeline GHA** — `opta-pipeline.yml` for running Opta RAPM/SPM
  on GitHub Actions with auto cache upload
- **Bootstrap script** — `data-raw/bootstrap.R` for one-command fresh
  clone setup (data + models + caches)
- **Predictions pipeline** — Skills-based team features (step 02b), blog
  dispatch guard (`if: success()`)

### Bug Fixes & Hardening

- Fixed [`library()`](https://rdrr.io/r/base/library.html) vs
  [`requireNamespace()`](https://rdrr.io/r/base/ns-load.html) in PSOCK
  parallel workers — workers need
  [`library()`](https://rdrr.io/r/base/library.html) to attach packages
- Fixed [`on.exit()`](https://rdrr.io/r/base/on.exit.html) calls to use
  `add = TRUE` to prevent DuckDB connection leaks
- Pipeline joins migrated from `player_name` to `player_id` (one legacy
  join remains in `06_xrapm.R`)
- `filter_bad_xg_data` threshold set to 30% for Opta (25% zero-xG
  splints is normal with SPADL)

### Tests

- 35 test files, 2248+ expectations across loaders, models, pipelines,
  scraping, and value metrics

## panna 0.2.0

Major expansion: Opta is now the primary data source with full pipeline
support across 15 leagues.

### Breaking Changes

- `load_opta_*()` functions now default to `source = "remote"` instead
  of `"local"`. Existing code that relied on the default loading from
  local files will now download from GitHub. Add `source = "local"`
  explicitly to restore the old behavior.
  ([\#11](https://github.com/peteowen1/panna/issues/11))
- [`player_skill_profile()`](https://peteowen1.github.io/panna/reference/player_skill_profile.md)
  now errors (with name suggestions) when a player is not found, instead
  of returning `NULL` with a warning. Code checking `is.null(result)`
  should use [`tryCatch()`](https://rdrr.io/r/base/conditions.html)
  instead. ([\#13](https://github.com/peteowen1/panna/issues/13))
- [`player_skill_profile()`](https://peteowen1.github.io/panna/reference/player_skill_profile.md)
  return columns changed: `weighted_90s` and `confidence` removed; new
  columns `type`, `raw_avg`, `n90`, `w90`, `attempts`, `w_attempts`
  added. ([\#13](https://github.com/peteowen1/panna/issues/13))
- [`to_opta_league()`](https://peteowen1.github.io/panna/reference/to_opta_league.md)
  now errors on unknown league codes when the catalog is available
  (previously warned and passed through). Typos like `"EPLL"` now fail
  fast. ([\#11](https://github.com/peteowen1/panna/issues/11))
- [`suggest_opta_seasons()`](https://peteowen1.github.io/panna/reference/suggest_opta_seasons.md)
  is no longer exported (now internal). Use
  [`list_opta_seasons()`](https://peteowen1.github.io/panna/reference/list_opta_seasons.md)
  instead. ([\#11](https://github.com/peteowen1/panna/issues/11))

### User Experience Improvements

- `load_opta_*()` functions now default to `source = "remote"`, so data
  loads directly from GitHub without requiring
  [`pb_download_opta()`](https://peteowen1.github.io/panna/reference/pb_download_opta.md)
  first ([\#11](https://github.com/peteowen1/panna/issues/11))
- [`to_opta_league()`](https://peteowen1.github.io/panna/reference/to_opta_league.md)
  now accepts case-insensitive input: “epl”, “eng”, “Eng” all work
  ([\#11](https://github.com/peteowen1/panna/issues/11))
- [`list_opta_seasons()`](https://peteowen1.github.io/panna/reference/list_opta_seasons.md)
  and
  [`list_opta_leagues()`](https://peteowen1.github.io/panna/reference/list_opta_leagues.md)
  now accept `source = "remote"` as an alias for `"catalog"` for
  consistency with `load_opta_*()` functions
  ([\#11](https://github.com/peteowen1/panna/issues/11))
- Local-only error messages now suggest `source = 'remote'` as an
  alternative ([\#11](https://github.com/peteowen1/panna/issues/11))
- [`player_skill_profile()`](https://peteowen1.github.io/panna/reference/player_skill_profile.md)
  auto-loads pre-computed skills and match stats from GitHub releases
  when called with no data, instead of downloading ~200 MB of raw stats
  ([\#13](https://github.com/peteowen1/panna/issues/13))
- New
  [`load_opta_skills()`](https://peteowen1.github.io/panna/reference/load_opta_skills.md)
  function for loading pre-computed skill estimates from GitHub releases
  ([\#13](https://github.com/peteowen1/panna/issues/13))
- Corrupt parquet files from interrupted downloads are now detected and
  re-downloaded automatically
  ([\#12](https://github.com/peteowen1/panna/issues/12))
- DuckDB “No magic bytes” errors now give a clear message and
  auto-remove the corrupt cache
  ([\#12](https://github.com/peteowen1/panna/issues/12))
- Fixed [`on.exit()`](https://rdrr.io/r/base/on.exit.html) calls to use
  `add = TRUE` to prevent DuckDB connection leaks
  ([\#12](https://github.com/peteowen1/panna/issues/12))

### Opta RAPM/SPM Pipeline

- Full RAPM/SPM/Panna rating pipeline for Opta data (15 leagues, 42K+
  matches)
- Opta SPM uses 80+ features including xMetrics enrichment
- Parallel pipeline scripts in `data-raw/player-ratings-opta/`
- 15 leagues: Big 5 + NED/POR/TUR/ENG2/SCO + UCL/UEL/UECL + WC/EURO

### EPV (Expected Possession Value)

- Action-level player valuation from Opta event data with x/y
  coordinates
- SPADL conversion for Opta events
  ([`convert_opta_to_spadl()`](https://peteowen1.github.io/panna/reference/convert_opta_to_spadl.md))
- XGBoost models for xG, xPass, P(scoring), P(conceding)
- EPV credit assignment with pass credit splitting
- Pre-trained models stored in GitHub Releases

### xMetrics Pipeline

- Pre-computed xG/xA/xPass metrics for all 15 Opta leagues
- Uses pre-trained SPADL + XGBoost models (penalty xG overridden to
  0.76)
- Output as parquet files per league/season
- Loaded via
  [`load_opta_xmetrics()`](https://peteowen1.github.io/panna/reference/load_opta_xmetrics.md)

### Estimated Skills

- Bayesian decay-weighted skill estimation with exponential recency
  weighting
- Position-specific prior multipliers (GK/DEF/MID/FWD)
- 3-pass optimization of prior strength, lambda, and quantile
- Context adjustments for opponent quality, venue, and league level
- Player skill profiles with percentiles and confidence intervals
- Backtest framework for evaluating prediction accuracy
- New functions:
  [`estimate_player_skills()`](https://peteowen1.github.io/panna/reference/estimate_player_skills.md),
  [`player_skill_profile()`](https://peteowen1.github.io/panna/reference/player_skill_profile.md),
  [`backtest_skill_predictions()`](https://peteowen1.github.io/panna/reference/backtest_skill_predictions.md),
  [`optimize_all_priors()`](https://peteowen1.github.io/panna/reference/optimize_all_priors.md)

### Match Predictions

- XGBoost Poisson model for home/away goal counts
- XGBoost multinomial model for W/D/L probabilities
- Elo rating system with
  [`init_team_elos()`](https://peteowen1.github.io/panna/reference/init_team_elos.md),
  [`compute_match_elos()`](https://peteowen1.github.io/panna/reference/compute_match_elos.md)
- Rolling team form features (5/10/20 match windows)
- Team-level skill feature aggregation from estimated skills
- Calibration and logloss evaluation tools

### New Opta Data Loaders

- [`load_opta_match_events()`](https://peteowen1.github.io/panna/reference/load_opta_match_events.md) -
  All events with x/y coordinates
- [`load_opta_lineups()`](https://peteowen1.github.io/panna/reference/load_opta_lineups.md) -
  Lineup data
- [`load_opta_fixtures()`](https://peteowen1.github.io/panna/reference/load_opta_fixtures.md) -
  Fixture/results data with match status filtering
- [`load_opta_xmetrics()`](https://peteowen1.github.io/panna/reference/load_opta_xmetrics.md) -
  Pre-computed xG/xA/xPass metrics
- [`load_opta_shot_events()`](https://peteowen1.github.io/panna/reference/load_opta_shot_events.md) -
  Individual shots with coordinates
- [`load_opta_events()`](https://peteowen1.github.io/panna/reference/load_opta_events.md) -
  Goals, cards, substitutions
- [`list_opta_leagues()`](https://peteowen1.github.io/panna/reference/list_opta_leagues.md) -
  Automatic league discovery from data catalog
- [`load_opta_match_stats()`](https://peteowen1.github.io/panna/reference/load_opta_match_stats.md) -
  Load pre-computed match-level statistics

### Other Improvements

- Opta column count corrected to 263 (was incorrectly documented as 271)
- Opta xG model documented as SPADL + XGBoost (was incorrectly shown as
  “None”)
- Opta history starts 2013+ (was incorrectly shown as 2010+)

## panna 0.1.0

Initial release of the panna player rating system.

### Rating System

- RAPM (Regularized Adjusted Plus-Minus) implementation using
  splint-based analysis
- SPM (Statistical Plus-Minus) for box score prediction of RAPM
- Combined Panna ratings with SPM as Bayesian prior
- Offensive and defensive rating decomposition

### Data Sources

- FBref support with StatsBomb xG (Big 5 leagues, cups, international)
- Opta support with 263 columns per player (15 leagues, 2013+)
- Understat support with xGChain and xGBuildup (Big 5 + Russia)

### Data Loading

- DuckDB-based efficient parquet loading
- `load_summary()`, `load_passing()`, `load_defense()`,
  `load_possession()`, `load_shots()`, `load_metadata()` for FBref
- [`load_opta_stats()`](https://peteowen1.github.io/panna/reference/load_opta_stats.md),
  [`load_opta_shots()`](https://peteowen1.github.io/panna/reference/load_opta_shots.md),
  [`load_opta_big5()`](https://peteowen1.github.io/panna/reference/load_opta_big5.md)
  for Opta
- `load_understat_roster()`, `load_understat_shots()` for Understat

### Player Statistics

- Aggregated player statistics with `player_fbref_*()`,
  `player_opta_*()`, `player_understat_*()` functions
- Filtering by minimum minutes, leagues, and seasons

### Data Distribution

- GitHub Releases integration via piggyback
- [`pb_download_source()`](https://peteowen1.github.io/panna/reference/pb_download_source.md)
  for data download
- [`pb_upload_parquet()`](https://peteowen1.github.io/panna/reference/pb_upload_parquet.md)
  for data upload
