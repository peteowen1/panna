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
| FBref | Big 5 leagues + cups + international | StatsBomb | Most comprehensive passing |
| Opta | 15 leagues, 2013+ | SPADL xG (pre-trained XGBoost) | 263 columns, progressive carries, set pieces |
| Understat | Big 5 + Russia | Understat model | xGChain, xGBuildup |

### League Codes
- **Big 5 Panna → Opta**: ENG→EPL, ESP→La_Liga, GER→Bundesliga, ITA→Serie_A, FRA→Ligue_1
- **Extended domestic**: NED→Eredivisie, POR→Primeira_Liga, TUR→Super_Lig, ENG2→Championship, SCO→Scottish_Premiership
- **European**: UCL, UEL, UECL→Conference_League
- **International**: WC→World_Cup, EURO→UEFA_Euros
- Season format: FBref/Opta domestic uses "2024-2025", Understat uses "2024", tournaments use "YYYY Country" (e.g., "2018 Russia")

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

### RStudio Document Outline
All `data-raw/` scripts must use RStudio document outline formatting for navigation:

```r
# script_name.R
# Brief description of what this script does
#
# Additional details if needed

# 1. Setup ----

library(dplyr)
devtools::load_all()

# 2. Configuration ----

# Configuration code here...

# 3. Main Processing ----

# Main code here...

# 4. Summary ----

# Summary/output code here...
```

**Rules:**
- Use `# Section Name ----` format (4+ trailing dashes, equals, or pound signs)
- Number sections sequentially (`# 1. Setup ----`, `# 2. Configuration ----`, etc.)
- Keep section names concise and descriptive
- First lines should be script name and description as comments
- Do NOT use banner-style comments (`# ===...===`) as section headers - they don't appear in RStudio's outline

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

**Opta RAPM/SPM Pipeline** (`data-raw/player-ratings-opta/`):
```
Same 01-08 structure as FBref pipeline but using Opta data
run_pipeline_opta.R orchestrates the full run (15 leagues)
Uses aggregate_opta_stats() for SPM features + optional xMetrics enrichment
```

**EPV Pipeline** (`data-raw/epv/`):
```
Opta match events (load_opta_match_events)
       ↓
01_train_epv_models.R → xg_model.rds, xpass_model.rds, epv_model.rds
       ↓
02_calculate_player_epv.R → player_epv_{league}_{season}.rds
       ↓
03_calculate_player_xmetrics.R → xmetrics/{league}/{season}.parquet
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
- **Aerials are filtered out** at SPADL conversion due to `end_x=0` data issue (see ENHANCEMENTS.md)

### Key Modules

| Module | Non-obvious details |
|--------|---------|
| `data_loaders.R` | Uses DuckDB for parquet reads; `source = "remote"` (default) vs `"local"` |
| `opta_loaders.R` | League code conversion (ENG→EPL etc.), xmetrics loading |
| `splint_creation.R` | Also contains Opta adapter functions (`prepare_opta_*_for_splints()`) |
| `spm_opta.R` | `aggregate_opta_stats()` produces 80+ derived features from 263 raw columns |
| `xg_model.R` | Also has `derive_xa()` and `aggregate_player_xmetrics()` |
| `spadl_conversion.R` | Drops `original_event_id` - use composite keys to match back to raw events |
| `constants.R` | All magic numbers centralized here (penalty xG = 0.76, lambda ranges, etc.) |

## Reference Documentation

- `DATA_DICTIONARY.md` - Processed data column definitions at each pipeline stage
- `OPTA_REFERENCE.md` - Opta event type_ids and qualifier_ids used in EPV pipeline
- `pannadata/DATA_DICTIONARY.md` - Raw data column definitions
