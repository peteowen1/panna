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
  source = c("remote", "local")
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

  Character. "remote" (default) or "local".

## Value

Data frame with columns: player, team, matches, minutes, saves,
saves_ibox, saves_obox, goals_conceded, goals_conceded_ibox,
shots_conceded_ibox, shots_conceded_obox, clean_sheets, diving_saves,
high_claims, punches, big_chance_saves, shots_conceded, save_pct,
goals_against_per90, shots_conceded_per90, clean_sheet_pct

## See also

Other player statistics:
[`compare_players()`](https://peteowen1.github.io/panna/reference/compare_players.md),
[`player_opta_chains()`](https://peteowen1.github.io/panna/reference/player_opta_chains.md),
[`player_opta_defense()`](https://peteowen1.github.io/panna/reference/player_opta_defense.md),
[`player_opta_passing()`](https://peteowen1.github.io/panna/reference/player_opta_passing.md),
[`player_opta_possession()`](https://peteowen1.github.io/panna/reference/player_opta_possession.md),
[`player_opta_setpiece()`](https://peteowen1.github.io/panna/reference/player_opta_setpiece.md),
[`player_opta_shots()`](https://peteowen1.github.io/panna/reference/player_opta_shots.md),
[`player_opta_summary()`](https://peteowen1.github.io/panna/reference/player_opta_summary.md),
[`player_opta_xg()`](https://peteowen1.github.io/panna/reference/player_opta_xg.md),
[`player_opta_xpass()`](https://peteowen1.github.io/panna/reference/player_opta_xpass.md)
