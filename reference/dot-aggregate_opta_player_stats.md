# Shared aggregation helper for player_opta\_\* functions

Shared aggregation helper for player_opta\_\* functions

## Usage

``` r
.aggregate_opta_player_stats(
  player,
  league,
  season,
  min_minutes,
  by_team,
  source,
  col_spec,
  derive_fn,
  col_order,
  loader = .load_opta_data
)
```

## Arguments

- player:

  Character. Player name filter (case-insensitive substring match).

- league:

  Character. League code (NULL for all leagues).

- season:

  Character. Season string.

- min_minutes:

  Numeric. Minimum minutes threshold.

- by_team:

  Logical. Aggregate by player+team if TRUE.

- source:

  Character. "remote" or "local".

- col_spec:

  Named list mapping output column names to source column names.

- derive_fn:

  Function(result) -\> result that adds derived metrics.

- col_order:

  Character vector of final column ordering.

- loader:

  Function(league, season, source) for data loading.

## Value

data.frame with aggregated player statistics.
