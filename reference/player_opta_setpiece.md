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

## See also

Other player statistics:
[`compare_players()`](https://peteowen1.github.io/panna/reference/compare_players.md),
[`player_opta_chains()`](https://peteowen1.github.io/panna/reference/player_opta_chains.md),
[`player_opta_defense()`](https://peteowen1.github.io/panna/reference/player_opta_defense.md),
[`player_opta_keeper()`](https://peteowen1.github.io/panna/reference/player_opta_keeper.md),
[`player_opta_passing()`](https://peteowen1.github.io/panna/reference/player_opta_passing.md),
[`player_opta_possession()`](https://peteowen1.github.io/panna/reference/player_opta_possession.md),
[`player_opta_shots()`](https://peteowen1.github.io/panna/reference/player_opta_shots.md),
[`player_opta_summary()`](https://peteowen1.github.io/panna/reference/player_opta_summary.md),
[`player_opta_xg()`](https://peteowen1.github.io/panna/reference/player_opta_xg.md),
[`player_opta_xpass()`](https://peteowen1.github.io/panna/reference/player_opta_xpass.md)
