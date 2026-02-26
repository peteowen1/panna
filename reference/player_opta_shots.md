# Opta Player Shooting Statistics

Aggregate shooting statistics from Opta data. Includes shot volume, shot
locations, finishing by body part, and big chances.

## Usage

``` r
player_opta_shots(
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

Data frame with shooting statistics.
