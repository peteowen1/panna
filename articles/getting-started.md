# Getting Started with panna

This vignette covers installation, data download, and basic usage of the
panna package.

## Installation

Install panna from GitHub:

``` r
# Install devtools if needed
install.packages("devtools")

# Install panna
devtools::install_github("peteowen1/panna")
```

## Downloading Data

Data is stored in GitHub Releases. Most `load_*()` functions default to
`source = "remote"`, so you can start using data immediately. For faster
repeated access, download data locally first:

``` r
library(panna)

# Download Opta data (the project's sole data source)
pb_download_source("opta")
```

Data is cached locally after download, so you only need to do this once
(or when updating to get new matches).

## Loading Data

### Opta Data

Opta is the package’s data source, with 263 columns per player across 15
leagues. It powers the full RAPM/SPM pipeline, EPV, xMetrics, estimated
skills, and match predictions.

``` r
# Load Opta player stats
opta_stats <- load_opta_stats("EPL", "2024-2025")

# Load Opta shots
opta_shots <- load_opta_shots("EPL", "2024-2025")

# Load all Big 5 leagues at once
big5 <- load_opta_big5("2024-2025")

# Additional Opta data types
match_events <- load_opta_match_events("EPL", "2024-2025")  # All events with x/y
lineups <- load_opta_lineups("EPL", "2024-2025")             # Lineup data
fixtures <- load_opta_fixtures("EPL")                        # Fixtures/results
xmetrics <- load_opta_xmetrics("EPL", "2024-2025")           # Pre-computed xG/xA/xPass
```

## League and Season Codes

### Opta League Codes (15 leagues)

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

### Season Formats

| Type             | Format       | Example       |
|------------------|--------------|---------------|
| Domestic leagues | YYYY-YYYY    | “2024-2025”   |
| Tournaments      | YYYY Country | “2018 Russia” |

## Aggregated Player Statistics

The package provides convenience functions for aggregated player
statistics:

``` r
# Opta aggregated summary stats
opta_players <- player_opta_summary(
  leagues = "EPL",
  seasons = "2024-2025",
  min_minutes = 900  # Filter for players with 900+ minutes
)

# Other aggregated stat tables (same arguments)
players_passing <- player_opta_passing(
  leagues = c("EPL", "La_Liga"),               # Multiple leagues
  seasons = c("2023-2024", "2024-2025"),       # Multiple seasons
  min_minutes = 1800
)

players_defense <- player_opta_defense(leagues = "EPL", seasons = "2024-2025")
players_shots   <- player_opta_shots(leagues = "EPL", seasons = "2024-2025")
```

## Working with Multiple Leagues

Many functions accept vectors of leagues and seasons:

``` r
# Load Big 5 Opta stats
big5_opta <- load_opta_big5("2024-2025")

# Or load specific leagues individually
multi_league <- data.table::rbindlist(lapply(
  c("EPL", "La_Liga", "Bundesliga"),
  function(lg) load_opta_stats(lg, "2024-2025")
), fill = TRUE)
```

## Advanced Features

Beyond basic data loading and player statistics, panna includes several
advanced analytical pipelines:

- **EPV (Expected Possession Value)**: Action-level player valuation
  from Opta event data with x/y coordinates. See
  [`?calculate_action_epv`](https://peteowen1.github.io/panna/reference/calculate_action_epv.md).
- **Estimated Skills**: Bayesian decay-weighted skill estimation with
  position-specific priors and context adjustments. See
  [`?estimate_player_skills`](https://peteowen1.github.io/panna/reference/estimate_player_skills.md)
  and
  [`?player_skill_profile`](https://peteowen1.github.io/panna/reference/player_skill_profile.md).
- **Match Predictions**: XGBoost Poisson/multinomial models for match
  outcome prediction using Elo, rolling form, and team-level skill
  features. See
  [`?predict_match`](https://peteowen1.github.io/panna/reference/predict_match.md).

These pipelines are built on top of Opta data and require running the
RAPM/SPM pipeline first.

## Next Steps

- [Player
  Ratings](https://peteowen1.github.io/panna/articles/player-ratings.md) -
  Learn about RAPM and SPM methodology
- [Data
  Sources](https://peteowen1.github.io/panna/articles/data-sources.md) -
  Opta league codes, season formats, and loaders
- [Data
  Dictionary](https://peteowen1.github.io/panna/DATA_DICTIONARY.md) -
  Column definitions
