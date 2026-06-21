# Data Sources Guide

This vignette is the reference for panna’s data source: Opta. It covers
league codes, season formats, the loader functions, and the statistics
available per match.

## Overview

panna is built exclusively on Opta data. Opta powers the full RAPM/SPM
rating pipeline, EPV, xMetrics, estimated skills, and match predictions
across 15 leagues and international tournaments.

| Feature     | Opta                                             |
|-------------|--------------------------------------------------|
| xG Model    | SPADL + XGBoost (panna’s own pre-trained model)  |
| Leagues     | 15 leagues + international tournaments           |
| History     | 2013+                                            |
| Columns     | 263 per match                                    |
| Event data  | Full match events with x/y coordinates           |
| Primary Use | Full RAPM/SPM pipeline, EPV, skills, predictions |

### Strengths

- **15 leagues**: Big 5 + NED/POR/TUR/ENG2/SCO + UCL/UEL/UECL + WC/EURO
- **263 columns**: Detailed per-match player statistics
- **Event-level data**: Full match events with x/y coordinates for EPV
  and SPADL
- **Own xG model**: SPADL + XGBoost pre-trained model (no dependency on
  external xG)
- **Progressive metrics**: Carries, passes, receptions into penalty area
- **Set pieces**: Detailed breakdown of set piece involvement
- **xMetrics**: Pre-computed xG/xA/xPass for all leagues

## League Codes

``` r
# Big 5 leagues
"EPL"        # Premier League
"La_Liga"    # La Liga
"Bundesliga" # Bundesliga
"Serie_A"    # Serie A
"Ligue_1"    # Ligue 1

# Additional domestic leagues
"NED"   # Eredivisie
"POR"   # Primeira Liga
"TUR"   # Super Lig
"ENG2"  # Championship
"SCO"   # Scottish Premiership

# European competitions
"UCL"   # Champions League
"UEL"   # Europa League
"UECL"  # Conference League

# International tournaments
"WC"    # World Cup (e.g., "2018 Russia")
"EURO"  # European Championship (e.g., "2024 Germany")
```

## Season Format

Domestic leagues use a two-year season string; international tournaments
use a “year country” string:

``` r
# Domestic leagues: "YYYY-YYYY"
stats <- load_opta_stats("EPL", "2024-2025")

# Tournaments: "YYYY Country"
wc <- load_opta_stats("WC", "2018 Russia")
```

Note: calendar-year leagues (e.g. MLS/Argentina/Brazil) use a
single-year label such as `"2024"`. Always subset seasons by the season
end year rather than by an exact string match.

## Loading Opta Data

``` r
library(panna)

# Download (first time)
pb_download_source("opta")

# Player match stats (263 columns)
stats <- load_opta_stats("EPL", "2024-2025")

# Shot data
shots <- load_opta_shots("EPL", "2024-2025")

# All Big 5 leagues at once
big5 <- load_opta_big5("2024-2025")

# Event-level data (for EPV/SPADL)
match_events <- load_opta_match_events("EPL", "2024-2025")

# Lineups and fixtures
lineups <- load_opta_lineups("EPL", "2024-2025")
fixtures <- load_opta_fixtures("EPL")

# Pre-computed xG/xA/xPass metrics
xmetrics <- load_opta_xmetrics("EPL", "2024-2025")

# Aggregated player stats
players <- player_opta_summary(
  leagues = "EPL",
  seasons = "2024-2025",
  min_minutes = 900
)
```

## Aggregated Player Statistics

Several `player_opta_*()` helpers return per-player aggregates over one
or more leagues/seasons:

``` r
player_opta_summary("EPL", "2024-2025")      # Headline summary
player_opta_passing("EPL", "2024-2025")      # Passing breakdowns
player_opta_defense("EPL", "2024-2025")      # Tackles, interceptions, blocks
player_opta_possession("EPL", "2024-2025")   # Carries, receptions
player_opta_keeper("EPL", "2024-2025")       # Goalkeeper stats
player_opta_shots("EPL", "2024-2025")        # Shot-level aggregates
player_opta_xg("EPL", "2024-2025")           # xG / xA
player_opta_xpass("EPL", "2024-2025")        # xPass
player_opta_setpiece("EPL", "2024-2025")     # Set piece involvement
```

## Available Statistics

| Statistic           |        Opta         |
|---------------------|:-------------------:|
| Goals, Assists      |          Y          |
| xG, xA              | Y (SPADL + XGBoost) |
| npxG                |          Y          |
| Shots               |          Y          |
| Passing (basic)     |          Y          |
| Passing (distance)  |          Y          |
| Progressive passes  |          Y          |
| Progressive carries |          Y          |
| Tackles             |          Y          |
| Interceptions       |          Y          |
| Blocks              |          Y          |
| Aerial duels        |          Y          |
| Goalkeeper stats    |          Y          |
| Set piece detail    |          Y          |
| Event-level data    |   Y (x/y coords)    |

## Coverage

| League                     | Opta  |
|----------------------------|:-----:|
| Premier League             | 2013+ |
| La Liga                    | 2013+ |
| Bundesliga                 | 2013+ |
| Serie A                    | 2013+ |
| Ligue 1                    | 2013+ |
| Eredivisie (NED)           | 2013+ |
| Primeira Liga (POR)        | 2013+ |
| Super Lig (TUR)            | 2013+ |
| Championship (ENG2)        | 2013+ |
| Scottish Premiership (SCO) | 2019+ |
| Champions League           | 2013+ |
| Europa League              | 2013+ |
| Conference League          | 2021+ |
| World Cup                  | 2014+ |
| Euros                      | 2016+ |

## Next Steps

- [Getting
  Started](https://peteowen1.github.io/panna/articles/getting-started.md) -
  Installation and basic usage
- [Player
  Ratings](https://peteowen1.github.io/panna/articles/player-ratings.md) -
  RAPM and SPM methodology
- [Data
  Dictionary](https://peteowen1.github.io/panna/DATA_DICTIONARY.md) -
  Complete column definitions \`\`\`
