# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Package Overview

`panna` is an R package implementing player ratings for football (soccer) using the RAPM+SPM methodology. It calculates player impact on team performance using:
- **RAPM** (Regularized Adjusted Plus-Minus) - lineup-based impact from splint-level data
- **SPM** (Statistical Plus-Minus) - box score prediction of RAPM
- **Panna Rating** - combines RAPM with SPM as Bayesian prior for stability

The package works with FBref data via `worldfootballR` and supports Big 5 European leagues.

## Development Commands

```r
devtools::load_all()    # Load package for development
devtools::document()    # Generate documentation from roxygen2
devtools::test()        # Run all tests
testthat::test_file("tests/testthat/test-utils.R")  # Run single test file
devtools::check()       # Full package check
```

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

### worldfootballR Integration
- Prefer `load_*` functions (pre-scraped data) over scraping when available
- Pre-scraped data only has recent seasons (check `Season_End_Year`)
- Historical data requires direct scraping (respect rate limits)

## Architecture

### Core Data Flow
```
worldfootballR/FBref
       ↓
01_raw_data.rds (matches, lineups, events, shooting, stats)
       ↓
02_processed_data.rds (cleaned, standardized, match_id added)
       ↓
03_splints.rds (time segments with constant lineups)
       ↓
04_rapm.rds (base RAPM) ──→ 05_spm.rds (SPM model)
       ↓                            ↓
06_xrapm.rds (RAPM with SPM prior) ←┘
       ↓
07_panna.rds / panna_ratings.csv (final ratings)
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
| `scrape_fbref_data.R` | Data collection via worldfootballR |
| `scrape_fbref_direct.R` | Direct FBref HTTP scraping with caching |
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

### Data Collection
- `scrape_multi_league()` - Collect data across Big 5 leagues
- `scrape_pl_comprehensive()` - Single-league detailed collection

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

See `DATA_DICTIONARY.md` for complete column definitions at each pipeline stage.
