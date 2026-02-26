# Aggregate player statistics with optional team grouping

Common aggregation pattern for player stats functions. Aggregates
columns by player (and optionally team), adding most frequent team when
not grouping by team.

## Usage

``` r
aggregate_player_data(
  data,
  agg_cols,
  by_team = FALSE,
  player_col = "player",
  team_col = "team"
)
```

## Arguments

- data:

  Data frame with player-level data

- agg_cols:

  Named list where names are output column names and values are
  expressions to aggregate (as strings or column names)

- by_team:

  Logical. If TRUE, group by player and team. If FALSE, group by player
  only and add most frequent team.

- player_col:

  Name of player column (default "player")

- team_col:

  Name of team column (default "team")

## Value

Aggregated data frame with player, team, and aggregated columns

## Examples

``` r
if (FALSE) { # \dontrun{
# Aggregate goals and assists
aggregate_player_data(
  data = match_data,
  agg_cols = list(matches = "1", goals = "gls", assists = "ast"),
  by_team = FALSE
)
} # }
```
