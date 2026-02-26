# FBref Player Defense Statistics

Aggregate defensive statistics from FBref data. Returns totals and
per-90 rates.

## Usage

``` r
player_fbref_defense(
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
  NULL returns all players.

- league:

  Character. Filter by league code (ENG, ESP, GER, ITA, FRA).

- season:

  Character. Filter by season (e.g., "2024-2025").

- min_minutes:

  Integer. Minimum minutes for inclusion (default 450).

- by_team:

  Logical. If TRUE, aggregate by player and team separately. If FALSE
  (default), aggregate across all teams and show most common team.

- source:

  Character. "remote" (default) or "local".

## Value

Data frame with columns: player, team, matches, minutes, tackles,
tackles_won, interceptions, blocks, clearances, errors, tackles_per90,
interceptions_per90, blocks_per90, tackle_win_pct

## Examples

``` r
if (FALSE) { # \dontrun{
player_fbref_defense(league = "ENG", season = "2024-2025")
} # }
```
