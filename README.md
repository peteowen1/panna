# panna

<!-- badges: start -->
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

Player ratings for football (soccer) using RAPM+SPM methodology. Calculates player impact on team performance by combining lineup-based analysis with box score predictions.

## Overview

**panna** implements a three-stage rating system:

1. **RAPM** (Regularized Adjusted Plus-Minus) - Measures player impact from lineup data using ridge regression on "splints" (time segments with constant lineups)
2. **SPM** (Statistical Plus-Minus) - Predicts RAPM from box score statistics using elastic net
3. **Panna Rating** - Combines RAPM with SPM as a Bayesian prior for more stable ratings

Ratings are split into offensive and defensive components:
- **Offense**: Positive = helps create xG (good)
- **Defense**: Negative = prevents xG (good)
- **Overall**: `panna = offense - defense`

## Installation

```r
# Install from CRAN
install.packages("panna")

# Or install the development version from GitHub
devtools::install_github("peteowen1/panna")
```

## Quick Start

```r
library(panna)

# Download data from GitHub releases (first time only)
pb_download_source("fbref")

# Load player statistics for a league/season
summary_data <- load_summary("ENG", "2024-2025")

# Get aggregated player stats
player_stats <- player_fbref_summary(
 leagues = "ENG",
 seasons = "2024-2025",
 min_minutes = 900
)
```

## Data Sources

The package supports three football data providers:

| Source | Coverage | xG Model | Unique Features |
|--------|----------|----------|-----------------|
| FBref | Big 5 leagues + cups | StatsBomb | Most comprehensive passing stats |
| Opta | Big 5 leagues (2010+) | None | 271 columns, progressive carries |
| Understat | Big 5 + Russia | Understat | xGChain, xGBuildup |

### League Codes

| League | Panna Code | Opta Code | Season Format |
|--------|------------|-----------|---------------|
| Premier League | ENG | EPL | 2024-2025 |
| La Liga | ESP | La_Liga | 2024-2025 |
| Bundesliga | GER | Bundesliga | 2024-2025 |
| Serie A | ITA | Serie_A | 2024-2025 |
| Ligue 1 | FRA | Ligue_1 | 2024-2025 |

## Key Functions

### Data Loading

```r
# FBref data (via pannadata)
load_summary(league, season)
load_passing(league, season)
load_defense(league, season)
load_shots()

# Opta data
load_opta_stats(league, season)
load_opta_shots(league, season)

# Understat data
load_understat_roster(league, season)
load_understat_shots(league, season)
```

### Player Statistics

```r
# Aggregated stats with filters
player_fbref_summary(leagues, seasons, min_minutes)
player_fbref_passing(leagues, seasons, min_minutes)
player_opta_summary(leagues, seasons, min_minutes)
player_understat_summary(leagues, seasons, min_minutes)
```

### Rating Pipeline

```r
# Create splints from match data
splints <- create_all_splints(processed_data)

# Build RAPM design matrix
rapm_data <- create_rapm_design_matrix(splints)

# Fit RAPM model
rapm_results <- fit_rapm(rapm_data)

# Extract ratings
ratings <- extract_panna_ratings(rapm_results)
```

## Documentation

- [Getting Started](articles/getting-started.html) - Installation and basic usage
- [Player Ratings](articles/player-ratings.html) - RAPM and SPM methodology
- [Data Sources](articles/data-sources.html) - Choosing the right data source
- [Data Dictionary](DATA_DICTIONARY.md) - Column definitions for pipeline stages

## Related Packages

This package is part of the pannaverse ecosystem:

- **[pannaverse](https://github.com/peteowen1/pannaverse)** - Monorepo container
- **[pannadata](https://github.com/peteowen1/pannadata)** - Cached match data

## License

MIT
