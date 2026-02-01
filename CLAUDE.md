# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Package Overview

`panna` is an R package implementing player ratings for football (soccer) using the RAPM+SPM methodology. It calculates player impact on team performance using:
- **RAPM** (Regularized Adjusted Plus-Minus) - lineup-based impact from splint-level data
- **SPM** (Statistical Plus-Minus) - box score prediction of RAPM
- **Panna Rating** - combines RAPM with SPM as Bayesian prior for stability
- **EPV** (Expected Possession Value) - action-level player valuation from Opta event data

The package works with data from three sources via the `pannadata` repository and supports Big 5 European leagues.

## Development Commands

```r
devtools::load_all()    # Load package for development
devtools::document()    # Generate documentation from roxygen2
devtools::test()        # Run all tests
testthat::test_file("tests/testthat/test-utils.R")  # Run single test file
devtools::check()       # Full package check
```

## Data Sources

The package supports three football data sources with different strengths:

| Source | Coverage | xG Model | Unique Stats |
|--------|----------|----------|--------------|
| FBref | Big 5 leagues + more | StatsBomb | Most comprehensive passing |
| Opta | Big 5 leagues (2010-present) | None | 263 columns, progressive carries, set pieces |
| Understat | Big 5 + Russia | Understat model | xGChain, xGBuildup |

### League Codes
- **Panna codes**: ENG, ESP, GER, ITA, FRA
- **Opta codes**: EPL, La_Liga, Bundesliga, Serie_A, Ligue_1
- Season format: FBref uses "2024-2025", Understat uses "2024"

## Debugging

When debugging R code, write a script to the `debug/` folder and run it from there rather than using inline Rscript commands. This avoids issues with escaping and segfaults from complex inline commands.

```r
# Write debug script to debug/test.R then run:
Rscript debug/test.R
```

The `debug/` folder is gitignored.

## Coding Standards

### Column Naming
- **ALL column names must be snake_case**
- Use `janitor::clean_names()` via `clean_column_names()` helper
- Apply to all data frames before saving or returning

### Data Pipeline
- Package functions in `R/` - reusable, documented with roxygen2
- Analysis scripts in `data-raw/` - organized by pipeline:
  - `data-raw/player-ratings/` - RAPM/SPM/Panna rating pipeline (01-08 scripts)
  - `data-raw/epv/` - EPV model training and player valuation
  - `data-raw/scraping/` - Data collection scripts
  - `data-raw/analysis/` - Comparison and analysis scripts
- Cache expensive data to `data-raw/cache/`
- Analysis scripts run in RStudio - avoid excessive `cat()` calls, keep them simple

### pannadata Integration
- Use `panna::load_summary()`, `load_passing()`, etc. to load cached match data
- Data stored in `pannadata/data/{table_type}/{league}/{season}/{match_id}.rds`
- Column names are snake_case (via `janitor::clean_names()`)
- See `pannadata/DATA_DICTIONARY.md` for column definitions
- **DO NOT use worldfootballR** - pannadata has its own FBref scraping system

## Architecture

### Core Data Flow

**Player Ratings Pipeline** (`data-raw/player-ratings/`):
```
pannadata (cached match data from FBref/Opta/Understat)
       ↓
01_load_pannadata.R (loads summary, passing, defense, possession, shots, metadata)
       ↓
02_data_processing.R → processed_data.rds (cleaned, standardized)
       ↓
03_splint_creation.R → splints.rds (time segments with constant lineups)
       ↓
04_rapm.R → rapm.rds (base RAPM) ──→ 05_spm.R → spm.rds (SPM model)
       ↓                                    ↓
06_xrapm.R → xrapm.rds (RAPM with SPM prior) ←┘
       ↓
07_seasonal_ratings.R → seasonal ratings
       ↓
08_panna_ratings.R → panna_ratings.csv (final ratings)
```

**EPV Pipeline** (`data-raw/epv/`):
```
Opta match events (load_opta_match_events)
       ↓
01_train_epv_models.R → xg_model.rds, xpass_model.rds, epv_model.rds
       ↓
02_calculate_player_epv.R → player_epv_{league}_{season}.rds
```

### Key Concepts

**Splints**: Time segments where the lineup is constant. Boundaries created at goals, substitutions, red cards, and halftime. Each splint has ~22 players (11 per team) and calculates npxGD (non-penalty xG differential).

**RAPM Design Matrix**:
- 2 rows per splint (home attacking perspective, away attacking perspective)
- Player columns: `playerX_off` (offensive), `playerX_def` (defensive)
- Covariates: goal_diff, goals_for/against, avg_minute, home/away, player counts
- Target: `xgf90` (xG FOR per 90 minutes)
- Uses ridge regression with cross-validated lambda

**Rating Interpretation**:
- Offense coefficient: positive = helps create xG (good)
- Defense coefficient: positive = allows xG (bad), negative = prevents xG (good)
- Final: `panna = offense - defense` (higher = better)

**EPV (Expected Possession Value)**:
- Action-level valuation using Opta event data with x/y coordinates
- Based on VAEP/Goals Added methodology
- EPV = ΔP(scoring) - ΔP(conceding) for each action
- Pass credit split: passer gets (1-xPass) share, receiver gets xPass share
- Trains 4 XGBoost models: xG, xPass, P(scoring), P(conceding)

### Module Responsibilities

| Module | Purpose |
|--------|---------|
| `data_loaders.R` | DuckDB-based loading from local/remote parquet (FBref, Understat) |
| `opta_loaders.R` | Opta data loading with league code conversion |
| `player_stats.R` | User-facing aggregated stats functions (player_fbref_*, player_opta_*, player_understat_*) |
| `piggyback.R` | GitHub Releases upload/download (pb_upload_*, pb_download_*) |
| `data_processing.R` | Cleaning, standardization, merging |
| `splint_creation.R` | Time segment creation for RAPM + Opta adapters |
| `rapm_matrix.R` | Design matrix construction |
| `rapm_model.R` | Ridge regression model fitting |
| `spm_model.R` | Box score prediction model (FBref + Opta SPM) |
| `panna_rating.R` | Final rating calculation |
| `offensive_defensive.R` | O/D rating decomposition |
| `feature_engineering.R` | Advanced feature creation (per-100 sequences) |
| `utils.R` | Helpers: clean_column_names, safe_divide, per_90, validate_seasons |
| `globals.R` | NSE variable declarations for R CMD check |
| `spadl_conversion.R` | Opta events → SPADL format for EPV |
| `possession_chains.R` | Group actions into possession sequences |
| `xg_model.R` | XGBoost xG model (Opta has no xG) |
| `xpass_model.R` | Pass completion probability for credit split |
| `epv_features.R` | EPV game state features |
| `epv_model.R` | Main EPV training/prediction/aggregation |

## Key Functions

### Data Loading (FBref via pannadata)
- `load_summary()`, `load_passing()`, `load_defense()`, `load_possession()`, `load_shots()`, `load_metadata()`
- Use `source = "remote"` (default) or `source = "local"`

### Data Loading (Opta)
- `load_opta_stats()` - Player match stats (263 columns)
- `load_opta_shots()` - Shot data
- `load_opta_big5()` - All Big 5 leagues at once
- `pb_download_opta()` - Download Opta data from GitHub releases

### Data Loading (Understat)
- `load_understat_roster()` - Player stats with xGChain, xGBuildup
- `load_understat_shots()` - Shot-level data with xG
- `load_understat_metadata()` - Match metadata

### Player Statistics (User-Facing Aggregations)
- `player_fbref_summary()`, `player_fbref_passing()`, `player_fbref_defense()`, `player_fbref_keeper()`
- `player_opta_summary()`, `player_opta_passing()`, `player_opta_defense()`, `player_opta_possession()`, `player_opta_keeper()`, `player_opta_shots()`, `player_opta_setpiece()`
- `player_understat_summary()`

### Splint Creation
- `create_all_splints()` - Master function for all matches
- `create_splint_boundaries()` - Define splint boundaries at events

### Opta Adapters (for splint creation)
- `prepare_opta_events_for_splints()` - Convert Opta events to FBref-like format
- `prepare_opta_lineups_for_splints()` - Convert Opta lineups to FBref-like format
- `prepare_opta_shots_for_splints()` - Convert Opta shots to FBref-like format
- `create_opta_processed_data()` - Master function creating processed data from Opta sources
- `extract_season_from_date()` - Helper to extract season string from match date

### RAPM
- `create_rapm_design_matrix()` - Build X matrix with player/covariate columns
- `fit_rapm()` - Fit cross-validated ridge regression
- `extract_panna_ratings()` - Extract player coefficients

### SPM
- `aggregate_player_stats()` - Convert FBref match stats to per-90 rates
- `fit_spm_model()` - Elastic net predicting RAPM from box scores
- `calculate_spm_ratings()` - Generate SPM for all players

### Opta SPM
- `aggregate_opta_stats()` - Aggregate Opta 263 columns to per-90 rates with derived features
- `fit_spm_opta()` - Elastic net on Opta features predicting RAPM
- `compare_spm_features()` - Compare feature importance between FBref and Opta SPM models

### Panna
- `fit_panna_model()` - End-to-end pipeline combining RAPM + SPM
- `fit_rapm_with_prior()` - RAPM shrinking toward SPM (xRAPM)

### EPV (Expected Possession Value)
**Data Preparation:**
- `convert_opta_to_spadl()` - Convert Opta match events to SPADL format
- `create_possession_chains()` - Group actions into possession sequences
- `classify_chain_outcomes()` - Label chains with goal/shot/turnover outcomes
- `label_actions_with_outcomes()` - Add chain labels to each action

**Model Components:**
- `fit_xg_model()` - Train XGBoost xG model on Opta shots
- `fit_xpass_model()` - Train pass completion probability model
- `fit_epv_scoring_model()` - P(scoring) during possession
- `fit_epv_conceding_model()` - P(conceding) on opponent's next possession

**Value Calculation:**
- `create_epv_features()` - Build game state features
- `calculate_action_epv()` - Compute EPV for each action
- `assign_pass_credit()` - Split pass value between passer/receiver
- `aggregate_player_epv()` - Summarize EPV by player (total, per-90, by action type)

**Model Storage:**
- `save_epv_models()` / `load_epv_models()` - Local model persistence
- `pb_download_epv_models()` - Download pre-trained models from GitHub releases

## Data Distribution (GitHub Releases)

Data is stored in GitHub Releases using tar.gz archives, organized by source:

| Release Tag | Archive | Contents |
|-------------|---------|----------|
| fbref-latest | fbref-parquet.tar.gz | FBref parquet files |
| understat-latest | understat-parquet.tar.gz | Understat parquet files |
| opta-latest | opta_player_stats.parquet, opta_shots.parquet | Consolidated Opta files |
| epv-models | xg_model.rds, xpass_model.rds, epv_*.rds | Pre-trained EPV models |

### Download Functions
```r
pb_download_source("fbref")     # Download FBref data
pb_download_source("understat") # Download Understat data
pb_download_opta()              # Download Opta data (consolidated files)
```

## Data Documentation

See `pannadata/DATA_DICTIONARY.md` for complete column definitions for raw data.
See `panna/DATA_DICTIONARY.md` for processed data column definitions at each pipeline stage.
See `panna/OPTA_REFERENCE.md` for Opta event type_ids and qualifier_ids used in EPV pipeline.
