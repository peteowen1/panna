# Changelog

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
- [`load_summary()`](https://peteowen1.github.io/panna/reference/load_summary.md),
  [`load_passing()`](https://peteowen1.github.io/panna/reference/load_passing.md),
  [`load_defense()`](https://peteowen1.github.io/panna/reference/load_defense.md),
  [`load_possession()`](https://peteowen1.github.io/panna/reference/load_possession.md),
  [`load_shots()`](https://peteowen1.github.io/panna/reference/load_shots.md),
  [`load_metadata()`](https://peteowen1.github.io/panna/reference/load_metadata.md)
  for FBref
- [`load_opta_stats()`](https://peteowen1.github.io/panna/reference/load_opta_stats.md),
  [`load_opta_shots()`](https://peteowen1.github.io/panna/reference/load_opta_shots.md),
  [`load_opta_big5()`](https://peteowen1.github.io/panna/reference/load_opta_big5.md)
  for Opta
- [`load_understat_roster()`](https://peteowen1.github.io/panna/reference/load_understat_roster.md),
  [`load_understat_shots()`](https://peteowen1.github.io/panna/reference/load_understat_shots.md)
  for Understat

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
