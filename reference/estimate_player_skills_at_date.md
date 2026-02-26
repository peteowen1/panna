# Estimate player skills at a specific date

Computes current skill vectors for specified players at a given date.
Useful for match prediction where you need skill estimates for upcoming
fixtures rather than end-of-season snapshots.

## Usage

``` r
estimate_player_skills_at_date(
  match_stats,
  decay_params = NULL,
  player_ids = NULL,
  date = Sys.Date(),
  min_weighted_90s = 5
)
```

## Arguments

- match_stats:

  A data.table from
  [`compute_match_level_opta_stats()`](https://peteowen1.github.io/panna/reference/compute_match_level_opta_stats.md).

- decay_params:

  Decay parameters.

- player_ids:

  Character vector of player_ids to estimate. If NULL, estimates all
  players with data before the date.

- date:

  Date to estimate skills at.

- min_weighted_90s:

  Regression threshold.

## Value

A data.table with one row per player containing skill estimates.
