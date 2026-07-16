# panna

Player ratings for football (soccer), built on Opta event and box-score
data. Full documentation site: <https://peteowen1.github.io/panna/>.

## Overview

panna produces two families of numbers (see
[`vignette("player-ratings")`](https://peteowen1.github.io/panna/articles/player-ratings.md)
for the full map):

- **Ratings** – forward-looking skill estimates: `panna` (career trait,
  decay-weighted xRAPM), season xRAPM, EPR, PSR, and per-stat estimated
  skills.
- **Production** – what actually happened: per-action EPV and WPA,
  per-game PSV, rolled up into `piero_value` (per match).

**Piero**, the headline blog rating, is a z-scored blend of `panna`
(0.5) / EPR (0.3) / PSR (0.2). RAPM (ridge regression on lineup
“splints”) and SPM (an XGBoost box-score prior) are the estimators
underneath xRAPM/panna; see
[`vignette("pipeline-walkthrough")`](https://peteowen1.github.io/panna/articles/pipeline-walkthrough.md)
(“Pipeline Anatomy”) for how each number is actually computed.

## Installation

``` r
# Install from GitHub
devtools::install_github("peteowen1/panna")
```

## Quick Start

``` r
library(panna)

# Latest PSR (Player Skill Rating) leaderboard -- downloads a small
# pre-computed snapshot, no local pipeline run required
player_psr(n = 10)

# Look up a player's estimated-skill profile
player_skill_profile("Lionel Messi")

# Load raw Opta player statistics (loads from GitHub automatically)
opta_stats <- load_opta_stats("EPL", "2024-2025")
```

## Data Sources

The package uses Opta as its sole data provider:

| Source | Coverage           | xG Model        | Unique Features                                    |
|--------|--------------------|-----------------|----------------------------------------------------|
| Opta   | 15 leagues (2013+) | SPADL + XGBoost | 263 columns, progressive carries, event-level data |

### League Codes

#### Opta Leagues (15)

| League               | Opta Code  | Season Format |
|----------------------|------------|---------------|
| Premier League       | EPL        | 2024-2025     |
| La Liga              | La_Liga    | 2024-2025     |
| Bundesliga           | Bundesliga | 2024-2025     |
| Serie A              | Serie_A    | 2024-2025     |
| Ligue 1              | Ligue_1    | 2024-2025     |
| Eredivisie           | NED        | 2024-2025     |
| Primeira Liga        | POR        | 2024-2025     |
| Super Lig            | TUR        | 2024-2025     |
| Championship         | ENG2       | 2024-2025     |
| Scottish Premiership | SCO        | 2024-2025     |
| Champions League     | UCL        | 2024-2025     |
| Europa League        | UEL        | 2024-2025     |
| Conference League    | UECL       | 2024-2025     |
| World Cup            | WC         | 2018 Russia   |
| Euros                | EURO       | 2024 Germany  |

## Key Functions

### Data Loading

``` r
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
```

### Player Statistics

``` r
# Opta aggregated stats -- same argument shape across the whole family
player_opta_summary(player = NULL, league, season, min_minutes = 450)
player_opta_passing(player = NULL, league, season, min_minutes = 450)
player_opta_defense(player = NULL, league, season, min_minutes = 450)
player_opta_possession(player = NULL, league, season, min_minutes = 450)
player_opta_keeper(player = NULL, league, season, min_minutes = 450)
player_opta_shots(player = NULL, league, season, min_minutes = 450)
player_opta_setpiece(player = NULL, league, season, min_minutes = 450)

compare_players(c("Salah", "Haaland"))  # side-by-side across all of the above
```

### Ratings and Predictions

``` r
player_psr(player = "Salah")             # PSR leaderboard / single-player lookup
player_skill_profile("Kylian Mbappe")    # per-stat skill profile with percentiles
player_value("Kane")                     # per-match EPV/WPA/PSV value profile (needs local pipeline cache)

preds <- load_predictions(source = "remote")
fit_bt_ratings(preds)                    # back out a single team-strength number
```

For how these are computed, see
[`vignette("pipeline-walkthrough")`](https://peteowen1.github.io/panna/articles/pipeline-walkthrough.md)
(“Pipeline Anatomy”); for match prediction and World Cup simulation, see
[`vignette("match-prediction")`](https://peteowen1.github.io/panna/articles/match-prediction.md);
for downloading/publishing the underlying data, see
[`vignette("data-bus")`](https://peteowen1.github.io/panna/articles/data-bus.md).

## Documentation

Full site: <https://peteowen1.github.io/panna/>

- [Getting
  Started](https://peteowen1.github.io/panna/articles/getting-started.md) -
  Installation and basic usage
- [Player
  Ratings](https://peteowen1.github.io/panna/articles/player-ratings.md) -
  EPR, PSR, panna, and the Piero composite
- [Pipeline
  Anatomy](https://peteowen1.github.io/panna/articles/pipeline-walkthrough.md) -
  how ratings and predictions are computed
- [Match Prediction and Tournament
  Simulation](https://peteowen1.github.io/panna/articles/match-prediction.md) -
  reading and simulating match outcomes
- [Data Access and
  Publishing](https://peteowen1.github.io/panna/articles/data-bus.md) -
  downloading and publishing pipeline data
- [Data
  Sources](https://peteowen1.github.io/panna/articles/data-sources.md) -
  choosing the right data source
- [Data
  Dictionary](https://peteowen1.github.io/panna/DATA_DICTIONARY.md) -
  column definitions for pipeline stages

## Related Packages

This package is part of the pannaverse ecosystem:

- **[pannaverse](https://github.com/peteowen1/pannaverse)** - Monorepo
  container
- **[pannadata](https://github.com/peteowen1/pannadata)** - Cached match
  data

## License

MIT
