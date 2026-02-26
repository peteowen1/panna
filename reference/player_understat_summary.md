# Understat Player Summary Statistics

Aggregate statistics from Understat data. Includes Understat's xG model.

## Usage

``` r
player_understat_summary(
  player = NULL,
  league = NULL,
  season = NULL,
  min_minutes = 450,
  by_team = FALSE,
  source = c("remote", "local")
)
```

## Arguments

- player:

  Character. Player name to filter.

- league:

  Character. League code (ENG, ESP, GER, ITA, FRA, RUS).

- season:

  Character. Season in Understat format (e.g., "2024").

- min_minutes:

  Integer. Minimum minutes for inclusion.

- by_team:

  Logical. If TRUE, aggregate by player and team.

- source:

  Character. "remote" (default) or "local".

## Value

Data frame with player statistics including xG, xA, xGChain, xGBuildup.
