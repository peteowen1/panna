# Opta Player Set Piece Statistics

Aggregate set piece statistics from Opta data. Includes corners, free
kicks, penalties, and set piece involvement.

## Usage

``` r
player_opta_setpiece(
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

  Character. Player name to filter (case-insensitive partial match).

- league:

  Character. League code (ENG, ESP, GER, ITA, FRA).

- season:

  Character. Season string (e.g., "2024-2025").

- min_minutes:

  Integer. Minimum minutes for inclusion.

- by_team:

  Logical. If TRUE, aggregate by player and team.

- source:

  Character. "remote" (default) or "local".

## Value

Data frame with set piece statistics.
