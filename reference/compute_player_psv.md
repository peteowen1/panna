# Compute PSV from bundled coefficient files

Convenience wrapper that loads pre-trained coefficients and calls
[`calculate_psv_components`](https://peteowen1.github.io/panna/reference/calculate_psv_components.md).

## Usage

``` r
compute_player_psv(
  player_match_stats,
  min_adjust = TRUE,
  center = TRUE,
  target = c("xg", "goals")
)
```

## Arguments

- player_match_stats:

  Per-game player stats (one row per player per match).

- min_adjust:

  Logical. Minutes-adjust raw counts. Default `TRUE`.

- center:

  Logical. Center within each round. Default `TRUE`.

- target:

  One of `"xg"` (default) or `"goals"`.

## Value

A data.table with `psv`, `osv`, `dsv` columns.
