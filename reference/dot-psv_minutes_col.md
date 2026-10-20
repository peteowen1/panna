# Resolve the minutes column PSV scales by

The ONE place that decides which column
[`calculate_psv`](https://peteowen1.github.io/panna/reference/calculate_psv.md)
uses for its per-90 adjustment, its `center_weights = "minutes"` weights
and its `scale_to_minutes` factor: `minutes_played` when present, else
`total_minutes`, else `NULL`. Anything that must price in the SAME
minutes as a PSV it adds to (the game-logs league offset, panna#164)
should read the `psv_minutes` column `calculate_psv` returns rather than
re-deriving a minutes column of its own.

## Usage

``` r
.psv_minutes_col(dt)
```

## Arguments

- dt:

  A data.frame or data.table of per-game stats.

## Value

A single column name, or `NULL` when neither column exists.
