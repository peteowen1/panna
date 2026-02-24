# panna 0.2.0

Major expansion: Opta is now the primary data source with full pipeline support across 15 leagues.

## User Experience Improvements

* `load_opta_*()` functions now default to `source = "remote"`, so data loads directly from GitHub without requiring `pb_download_opta()` first (#11)
* `to_opta_league()` now accepts case-insensitive input: "epl", "eng", "Eng" all work (#11)
* `list_opta_seasons()` now defaults to `source = "catalog"` for immediate season discovery (#11)
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
