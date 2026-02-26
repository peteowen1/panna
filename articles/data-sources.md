# Data Sources Guide

This vignette compares the three data sources available in panna and
helps you choose the right one for your analysis.

## Overview

| Feature          | Opta                                             | Understat        | FBref                          |
|------------------|--------------------------------------------------|------------------|--------------------------------|
| xG Model         | SPADL + XGBoost                                  | Understat        | StatsBomb                      |
| Leagues          | 15 leagues                                       | Big 5 + Russia   | Big 5 + cups + international   |
| History          | 2013+                                            | 2014+            | 2017+                          |
| Columns          | 263 per match                                    | ~30              | ~70 per table                  |
| Update Frequency | Varies                                           | Daily            | Daily                          |
| Primary Use      | Full RAPM/SPM pipeline, EPV, skills, predictions | xGChain analysis | Passing analysis, cup coverage |

## Opta (Primary Source)

Opta is the primary data source for the panna rating system, powering
the full RAPM/SPM pipeline, EPV, xMetrics, estimated skills, and match
predictions across 15 leagues.

### Strengths

- **15 leagues**: Big 5 + NED/POR/TUR/ENG2/SCO + UCL/UEL/UECL + WC/EURO
- **263 columns**: Most detailed per-match player statistics
- **Event-level data**: Full match events with x/y coordinates for EPV
  and SPADL
- **Own xG model**: SPADL + XGBoost pre-trained model (no dependency on
  external xG)
- **Progressive metrics**: Carries, passes, receptions into penalty area
- **Set pieces**: Detailed breakdown of set piece involvement
- **xMetrics**: Pre-computed xG/xA/xPass for all leagues

### League Codes

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

### Loading Opta Data

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

### When to Use Opta

- Running the full RAPM/SPM/Panna rating pipeline
- EPV (Expected Possession Value) analysis
- Estimated skills and match predictions
- Research requiring progressive carries or set pieces
- Analysis across 15 leagues (beyond Big 5)
- Historical analysis (2013+)

## Understat

Understat provides unique metrics like xGChain and xGBuildup.

### Strengths

- **xGChain**: xG attributed to all players involved in attacking
  sequence
- **xGBuildup**: xG for buildup players (excluding shooter and assister)
- **Free and accessible**: No scraping restrictions
- **Russian Premier League**: Only source covering Russia

### Unique Statistics

    xGChain   = Sum of xG for all sequences a player was involved in
    xGBuildup = xGChain minus xG and xA (contribution beyond final actions)

These metrics help identify players who contribute to attacks without
always getting the final touch.

### League Codes

``` r
# Understat leagues
"EPL"         # Premier League
"La_Liga"     # La Liga
"Bundesliga"  # Bundesliga
"Serie_A"     # Serie A
"Ligue_1"     # Ligue 1
"RFPL"        # Russian Premier League
```

### Season Format

Understat uses single-year seasons:

``` r
# Understat: "2024" means 2024-25 season
roster <- load_understat_roster("EPL", "2024")

# vs Opta/FBref: "2024-2025"
stats <- load_opta_stats("EPL", "2024-2025")
```

### Loading Understat Data

``` r
# Download
pb_download_source("understat")

# Load player roster with xGChain, xGBuildup
roster <- load_understat_roster("EPL", "2024")

# Load shot-level data
shots <- load_understat_shots("EPL", "2024")

# Match metadata
metadata <- load_understat_metadata("EPL", "2024")

# Aggregated stats
players <- player_understat_summary(
  leagues = "EPL",
  seasons = "2024",
  min_minutes = 900
)
```

### When to Use Understat

- Analyzing buildup contribution (xGChain, xGBuildup)
- Russian Premier League coverage
- When StatsBomb xG isn’t required

## FBref

FBref provides comprehensive passing statistics with StatsBomb xG
values.

### Strengths

- **StatsBomb xG**: Industry-leading expected goals model
- **Comprehensive passing**: Short/medium/long breakdowns, progressive
  distance
- **Multiple tables**: Summary, passing, defense, possession, keeper,
  misc, shots
- **Competition coverage**: Big 5 leagues, European cups, domestic cups,
  international

### League Codes

``` r
# Big 5 leagues
"ENG"  # Premier League
"ESP"  # La Liga
"GER"  # Bundesliga
"ITA"  # Serie A
"FRA"  # Ligue 1

# European competitions
"UCL"  # Champions League
"UEL"  # Europa League

# Domestic cups
"FA_CUP", "EFL_CUP"      # England
"COPA_DEL_REY"           # Spain
"DFB_POKAL"              # Germany
"COPPA_ITALIA"           # Italy
"COUPE_DE_FRANCE"        # France

# International
"WC", "EURO", "COPA_AMERICA", "AFCON", "NATIONS_LEAGUE"
```

### Loading FBref Data

``` r
library(panna)

# Download (first time)
pb_download_source("fbref")

# Load by table type
summary <- load_summary("ENG", "2024-2025")
passing <- load_passing("ENG", "2024-2025")
defense <- load_defense("ENG", "2024-2025")
possession <- load_possession("ENG", "2024-2025")
keeper <- load_keeper("ENG", "2024-2025")
shots <- load_shots()  # All leagues/seasons

# Aggregated player stats
players <- player_fbref_summary(
  leagues = "ENG",
  seasons = "2024-2025",
  min_minutes = 900
)
```

### When to Use FBref

- Analyzing passing patterns (short/medium/long breakdowns)
- Comparing across competitions (league + cup)
- Working with StatsBomb xG specifically
- Cup and international match analysis

## Comparison Table

### Available Statistics

| Statistic           |        Opta         |   Understat   |     FBref     |
|---------------------|:-------------------:|:-------------:|:-------------:|
| Goals, Assists      |          Y          |       Y       |       Y       |
| xG, xA              | Y (SPADL + XGBoost) | Y (Understat) | Y (StatsBomb) |
| npxG                |          Y          |       Y       |       Y       |
| xGChain             |          N          |       Y       |       N       |
| xGBuildup           |          N          |       Y       |       N       |
| Shots               |          Y          |       Y       |       Y       |
| Passing (basic)     |          Y          |       N       |       Y       |
| Passing (distance)  |          Y          |       N       |       Y       |
| Progressive passes  |          Y          |       N       |       Y       |
| Progressive carries |          Y          |       N       |       Y       |
| Tackles             |          Y          |       N       |       Y       |
| Interceptions       |          Y          |       N       |       Y       |
| Blocks              |          Y          |       N       |       Y       |
| Aerial duels        |          Y          |       N       |       Y       |
| Goalkeeper stats    |          Y          |       N       |       Y       |
| Set piece detail    |          Y          |       N       |    Limited    |
| Event-level data    |   Y (x/y coords)    |       N       |       N       |

### Coverage

| League                     | Opta  | Understat | FBref |
|----------------------------|:-----:|:---------:|:-----:|
| Premier League             | 2013+ |   2014+   | 2017+ |
| La Liga                    | 2013+ |   2014+   | 2017+ |
| Bundesliga                 | 2013+ |   2014+   | 2017+ |
| Serie A                    | 2013+ |   2014+   | 2017+ |
| Ligue 1                    | 2013+ |   2014+   | 2017+ |
| Eredivisie (NED)           | 2013+ |     N     |   N   |
| Primeira Liga (POR)        | 2013+ |     N     |   N   |
| Super Lig (TUR)            | 2013+ |     N     |   N   |
| Championship (ENG2)        | 2013+ |     N     |   N   |
| Scottish Premiership (SCO) | 2019+ |     N     |   N   |
| Champions League           | 2013+ |     N     |   Y   |
| Europa League              | 2013+ |     N     |   Y   |
| Conference League          | 2021+ |     N     |   N   |
| World Cup                  | 2014+ |     N     |   Y   |
| Euros                      | 2016+ |     N     |   Y   |
| Russian PL                 |   N   |   2014+   |   N   |
| Domestic Cups              |   N   |     N     |   Y   |

## Choosing a Data Source

### Use Opta when:

- Running the RAPM/SPM/Panna rating pipeline (recommended)
- Using EPV, xMetrics, estimated skills, or match predictions
- You need 263 columns of granularity
- Analysis across 15 leagues (beyond Big 5)
- Progressive carry or set piece analysis
- Historical analysis (2013+)

### Use Understat when:

- Analyzing buildup contribution (xGChain, xGBuildup)
- Covering the Russian Premier League
- Quick analysis without specific xG model preference

### Use FBref when:

- Detailed passing analysis (short/medium/long breakdowns)
- Cup or international match analysis
- You specifically need StatsBomb xG

## Combining Sources

You can use multiple sources in the same analysis:

``` r
# Get Opta stats for RAPM
opta_summary <- player_opta_summary("EPL", "2024-2025")

# Get Understat for xGChain
understat_summary <- player_understat_summary("EPL", "2024")

# Merge by player name (careful with name matching)
combined <- merge(
  opta_summary,
  understat_summary[, c("player_name", "xg_chain", "xg_buildup")],
  by = "player_name",
  all.x = TRUE
)
```

Note: Player name matching across sources requires care due to different
naming conventions.

## Next Steps

- [Getting
  Started](https://peteowen1.github.io/panna/articles/getting-started.md) -
  Installation and basic usage
- [Player
  Ratings](https://peteowen1.github.io/panna/articles/player-ratings.md) -
  RAPM and SPM methodology
- [Data
  Dictionary](https://peteowen1.github.io/panna/DATA_DICTIONARY.md) -
  Complete column definitions
