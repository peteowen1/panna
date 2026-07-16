# Opta Player Possession Statistics

Aggregate possession and carrying statistics from Opta data.

## Usage

``` r
player_opta_possession(
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

Data frame with player possession statistics.

## See also

Other player statistics:
[`compare_players()`](https://peteowen1.github.io/panna/reference/compare_players.md),
[`player_opta_chains()`](https://peteowen1.github.io/panna/reference/player_opta_chains.md),
[`player_opta_defense()`](https://peteowen1.github.io/panna/reference/player_opta_defense.md),
[`player_opta_keeper()`](https://peteowen1.github.io/panna/reference/player_opta_keeper.md),
[`player_opta_passing()`](https://peteowen1.github.io/panna/reference/player_opta_passing.md),
[`player_opta_setpiece()`](https://peteowen1.github.io/panna/reference/player_opta_setpiece.md),
[`player_opta_shots()`](https://peteowen1.github.io/panna/reference/player_opta_shots.md),
[`player_opta_summary()`](https://peteowen1.github.io/panna/reference/player_opta_summary.md),
[`player_opta_xg()`](https://peteowen1.github.io/panna/reference/player_opta_xg.md),
[`player_opta_xpass()`](https://peteowen1.github.io/panna/reference/player_opta_xpass.md)
