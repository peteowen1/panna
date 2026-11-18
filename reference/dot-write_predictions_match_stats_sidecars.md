# Write the two 01_match_stats side files for the predictions run

Write the two 01_match_stats side files for the predictions run

## Usage

``` r
.write_predictions_match_stats_sidecars(
  match_stats,
  match_stats_path,
  recent_years = 2L
)
```

## Arguments

- match_stats:

  The full step-01 table, as saved to `match_stats_path`.

- match_stats_path:

  Path of the full table.

- recent_years:

  The recent file keeps rows whose season end year is at least the
  latest minus this, plus rows with no end year. 2 covers the current
  season and the year before it, which is what 10b keeps for a
  current-season run.

## Value

The minimum season end year in the recent file, invisibly.
