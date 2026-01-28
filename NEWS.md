# panna 0.1.0

Initial release of the panna player rating system.

## Rating System

* RAPM (Regularized Adjusted Plus-Minus) implementation using splint-based analysis
* SPM (Statistical Plus-Minus) for box score prediction of RAPM
* Combined Panna ratings with SPM as Bayesian prior
* Offensive and defensive rating decomposition

## Data Sources

* FBref support with StatsBomb xG (Big 5 leagues, cups, international)
* Opta support with 271 columns per player (Big 5 leagues, 2010+)
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
