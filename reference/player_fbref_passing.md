# FBref Player Passing Statistics

Aggregate passing statistics from FBref data. Returns totals and per-90
rates.

## Usage

``` r
player_fbref_passing(
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

Data frame with columns: player, team, matches, minutes,
passes_completed, passes_attempted, pass_pct, progressive_passes,
key_passes, passes_into_final_third, passes_into_penalty_area, crosses,
passes_per90, progressive_passes_per90, key_passes_per90

## Examples

``` r
if (FALSE) { # \dontrun{
player_fbref_passing(league = "ENG", season = "2024-2025")
} # }
```
