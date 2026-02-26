# Opta Player Keeper Statistics

Aggregate goalkeeper statistics from Opta data.

## Usage

``` r
player_opta_keeper(
  player = NULL,
  league = NULL,
  season = NULL,
  min_minutes = 450,
  by_team = FALSE,
  source = c("local", "remote")
)
```

## Arguments

- player:

  Character. Player name to filter (case-insensitive partial match).

- league:

  Character. League code (ENG, ESP, GER, ITA, FRA).

- season:

  Character. Season (e.g., "2024-2025").

- min_minutes:

  Integer. Minimum minutes for inclusion (default 450).

- by_team:

  Logical. If TRUE, aggregate by player and team separately.

- source:

  Character. "local" (default) or "remote".

## Value

Data frame with columns: player, team, matches, minutes, saves,
saves_ibox, saves_obox, goals_conceded, goals_conceded_ibox,
shots_conceded_ibox, shots_conceded_obox, clean_sheets, diving_saves,
high_claims, punches, big_chance_saves, shots_conceded, save_pct,
goals_against_per90, shots_conceded_per90, clean_sheet_pct
