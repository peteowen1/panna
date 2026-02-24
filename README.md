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
# Install from GitHub
devtools::install_github("peteowen1/panna")
```

## Quick Start

```r
library(panna)

# Download data from GitHub releases (first time only)
pb_download_source("opta")

# Load Opta player statistics for a league/season
opta_stats <- load_opta_stats("EPL", "2024-2025")

# Get aggregated player stats
player_stats <- player_opta_summary(
 leagues = "EPL",
 seasons = "2024-2025",
 min_minutes = 900
)
```

## Data Sources

The package supports three football data providers:

| Source | Coverage | xG Model | Unique Features |
|--------|----------|----------|-----------------|
| Opta | 15 leagues (2013+) | SPADL + XGBoost | 263 columns, progressive carries, event-level data |
| Understat | Big 5 + Russia | Understat | xGChain, xGBuildup |
| FBref | Big 5 leagues + cups | StatsBomb | Comprehensive passing stats |

### League Codes

#### Opta Leagues (15)

| League | Opta Code | Season Format |
|--------|-----------|---------------|
| Premier League | EPL | 2024-2025 |
| La Liga | La_Liga | 2024-2025 |
| Bundesliga | Bundesliga | 2024-2025 |
| Serie A | Serie_A | 2024-2025 |
| Ligue 1 | Ligue_1 | 2024-2025 |
| Eredivisie | NED | 2024-2025 |
| Primeira Liga | POR | 2024-2025 |
| Super Lig | TUR | 2024-2025 |
| Championship | ENG2 | 2024-2025 |
| Scottish Premiership | SCO | 2024-2025 |
| Champions League | UCL | 2024-2025 |
| Europa League | UEL | 2024-2025 |
| Conference League | UECL | 2024-2025 |
| World Cup | WC | 2018 Russia |
| Euros | EURO | 2024 Germany |

#### FBref Leagues

| League | Panna Code | Season Format |
|--------|------------|---------------|
| Premier League | ENG | 2024-2025 |
| La Liga | ESP | 2024-2025 |
| Bundesliga | GER | 2024-2025 |
| Serie A | ITA | 2024-2025 |
| Ligue 1 | FRA | 2024-2025 |

## Key Functions

### Data Loading

```r
# Opta data (15 leagues, 263 columns per player)
load_opta_stats(league, season)        # Player match stats
load_opta_shots(league, season)        # Shot data
load_opta_match_events(league, season) # All events with x/y coordinates
load_opta_lineups(league, season)      # Lineup data
load_opta_fixtures(league, season)     # Fixture/results data
load_opta_xmetrics(league, season)     # Pre-computed xG/xA/xPass metrics
load_opta_shot_events(league, season)  # Individual shots with coordinates
load_opta_events(league, season)       # Goals, cards, substitutions
load_opta_big5(season)                 # All Big 5 leagues at once

# Understat data
load_understat_roster(league, season)
load_understat_shots(league, season)

# FBref data (via pannadata)
load_summary(league, season)
load_passing(league, season)
load_defense(league, season)
load_shots()
```

### Player Statistics

```r
# Opta aggregated stats
player_opta_summary(leagues, seasons, min_minutes)
player_opta_passing(leagues, seasons, min_minutes)
player_opta_defense(leagues, seasons, min_minutes)
player_opta_possession(leagues, seasons, min_minutes)
player_opta_keeper(leagues, seasons, min_minutes)
player_opta_shots(leagues, seasons, min_minutes)
player_opta_setpiece(leagues, seasons, min_minutes)

# Understat/FBref aggregated stats
player_understat_summary(leagues, seasons, min_minutes)
player_fbref_summary(leagues, seasons, min_minutes)
player_fbref_passing(leagues, seasons, min_minutes)
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

### Advanced Features

#### EPV (Expected Possession Value)

Action-level player valuation from Opta event data with x/y coordinates:

```r
# Convert Opta events to SPADL format
spadl <- convert_opta_to_spadl(match_events)

# Train/load models
xg_model <- load_xg_model()
epv_model <- load_epv_model()

# Build features and calculate action-level EPV
features <- create_epv_features(spadl)
epv_values <- calculate_action_epv(spadl, features, epv_model, xg_model)
player_epv <- aggregate_player_epv(epv_values)
```

#### Estimated Skills

Bayesian decay-weighted skill estimation with position-specific priors:

```r
# Estimate player skills from historical match data
skills <- estimate_player_skills(player_stats, decay_params)

# Get a player's skill profile with percentiles
profile <- player_skill_profile("Bukayo Saka", skills = skills)

# Estimate skills at a specific date (for predictions)
date_skills <- estimate_player_skills_at_date(player_stats, "2025-01-15")
```

#### Match Predictions

XGBoost Poisson/multinomial models for match outcome prediction:

```r
# Compute team Elo ratings
elos <- compute_match_elos(match_results)

# Compute rolling form features
rolling <- compute_team_rolling_features(match_results)

# Fit prediction models
goals_model <- fit_goals_xgb(training_data)
outcome_model <- fit_outcome_xgb(training_data)

# Predict a fixture
prediction <- predict_match(fixture, goals_model, outcome_model)
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
