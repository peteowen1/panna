# Inspect skill estimate breakdown for a single stat

Shows the full Bayesian decomposition for every player: raw totals,
career average, prior center, prior weight, decay-weighted evidence, and
the final skill estimate.

## Usage

``` r
inspect_skill(
  stat_name,
  match_stats,
  decay_params = NULL,
  target_date = Sys.Date()
)
```

## Arguments

- stat_name:

  Name of the stat (e.g. `"goals_p90"`).

- match_stats:

  A data.table from
  [`compute_match_level_opta_stats()`](https://peteowen1.github.io/panna/reference/compute_match_level_opta_stats.md).

- decay_params:

  Decay parameters list.

- target_date:

  Date to estimate skills as of. Default today.

## Value

A data.table sorted by skill estimate (descending), one row per player.
