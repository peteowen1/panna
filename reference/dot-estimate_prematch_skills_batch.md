# Estimate pre-match skills at multiple dates (incremental batch version)

Highly optimized for sequential date estimation. Instead of
re-processing all historical data at each date, maintains running
cumulative sums that are decayed forward and incrementally updated with
new observations. Uses [`rowsum()`](https://rdrr.io/r/base/rowsum.html)
(C-level) for grouped matrix sums.

## Usage

``` r
.estimate_prematch_skills_batch(
  match_stats,
  ref_dates,
  decay_params = NULL,
  min_weighted_90s = 3,
  verbose = TRUE
)
```

## Arguments

- match_stats:

  Match-level stats (output of `compute_match_level_opta_stats`).

- ref_dates:

  Character or Date vector of dates to estimate skills at.

- decay_params:

  Decay parameters (default:
  [`get_default_decay_params()`](https://peteowen1.github.io/panna/reference/get_default_decay_params.md)).

- min_weighted_90s:

  Minimum weighted 90s for inclusion (default 3).

- verbose:

  Print progress (default TRUE).

## Value

Named list of data.tables (one per ref_date), keyed by date string. Each
table has one row per player with skill columns.

## Details

Complexity: O(N + D \* new_rows_per_date) instead of O(N \* D). For
typical data (~1M rows, 659 dates), this is ~100-300x faster.
