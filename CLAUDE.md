# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Package Overview

`panna` is an R package implementing player ratings for football (soccer) using the RAPM+SPM methodology. It calculates player impact on team performance using:
- **RAPM** (Regularized Adjusted Plus-Minus) - lineup-based impact from splint-level data
- **SPM** (Statistical Plus-Minus) - box score prediction of RAPM
- **Panna Rating** - combines RAPM with SPM as Bayesian prior for stability

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
| Opta | Big 5 leagues (2010-present) | None | 271 columns, progressive carries, set pieces |
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
- Analysis scripts in `data-raw/` - sequential workflow (01_, 02_, etc.)
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

### Module Responsibilities

| Module | Purpose |
|--------|---------|
| `pannadata_loader.R` | Load cached data from pannadata repository |
| `data_processing.R` | Cleaning, standardization, merging |
| `splint_creation.R` | Time segment creation for RAPM |
| `rapm_matrix.R` | Design matrix construction |
| `rapm_model.R` | Ridge regression model fitting |
| `spm_model.R` | Box score prediction model |
| `panna_rating.R` | Final rating calculation |
| `offensive_defensive.R` | O/D rating decomposition |
| `feature_engineering.R` | Advanced feature creation (per-100 sequences) |
| `utils.R` | Helpers: clean_column_names, safe_divide, validate_seasons, etc. |
| `globals.R` | NSE variable declarations for R CMD check |

## Key Functions

### Data Loading (from pannadata)
- `load_summary()` - Load player summary stats for league/season
- `load_passing()` - Load detailed passing stats
- `load_defense()` - Load defensive action stats
- `load_possession()` - Load possession/carrying stats
- `load_shots()` - Load shot-level data
- `load_metadata()` - Load match metadata

### Splint Creation
- `create_all_splints()` - Master function for all matches
- `create_splint_boundaries()` - Define splint boundaries at events

### RAPM
- `create_rapm_design_matrix()` - Build X matrix with player/covariate columns
- `fit_rapm()` - Fit cross-validated ridge regression
- `extract_panna_ratings()` - Extract player coefficients

### SPM
- `aggregate_player_stats()` - Convert match stats to per-90 rates
- `fit_spm_model()` - Elastic net predicting RAPM from box scores
- `calculate_spm_ratings()` - Generate SPM for all players

### Panna
- `fit_panna_model()` - End-to-end pipeline combining RAPM + SPM
- `fit_rapm_with_prior()` - RAPM shrinking toward SPM (xRAPM)

## Data Documentation

See `pannadata/DATA_DICTIONARY.md` for complete column definitions for raw data.
See `panna/DATA_DICTIONARY.md` for processed data column definitions at each pipeline stage.
