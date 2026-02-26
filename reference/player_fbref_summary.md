# FBref Player Summary Statistics

Aggregate offensive statistics from FBref data. Returns totals and
per-90 rates. Includes xG metrics from StatsBomb.

## Usage

``` r
player_fbref_summary(
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

Data frame with columns: player, team, matches, minutes, goals, assists,
shots, shots_on_target, xg, npxg, xag, sca, gca, goals_minus_xg,
goals_per90, xg_per90, npxg_per90, xag_per90

## Examples

``` r
if (FALSE) { # \dontrun{
# Get all players with 450+ minutes
player_fbref_summary(league = "ENG", season = "2024-2025")

# Get specific player
player_fbref_summary(player = "Salah", league = "ENG")

# Group by team (useful for players who transferred)
player_fbref_summary(league = "ENG", season = "2024-2025", by_team = TRUE)
} # }
```
