# Compute PSV from bundled coefficient files

Convenience wrapper that loads pre-trained coefficients and calls
[`calculate_psv_components`](https://peteowen1.github.io/panna/reference/calculate_psv_components.md).

## Usage

``` r
compute_player_psv(
  player_match_stats,
  min_adjust = TRUE,
  center = TRUE,
  target = c("xg", "goals", "blend"),
  scale_to_minutes = FALSE,
  exclude_efficiency = TRUE
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

  One of `"xg"` (default, xG differential), `"goals"` (goal
  differential), or `"blend"` (alpha\*xG + (1-alpha)\*goals — the
  displayed value model; falls back to `"xg"` until the blend is
  trained).

- scale_to_minutes:

  Logical. Multiply the per-90 PSV by `minutes_played / 90` so values
  are additive over games (like EPV). Default `FALSE`. See
  [`calculate_psv`](https://peteowen1.github.io/panna/reference/calculate_psv.md).

- exclude_efficiency:

  Logical. Exclude efficiency/ratio stats. Default `TRUE`. Set `FALSE`
  to score with the full trained coefficient vector (the form used for
  the displayed blog PSV). See
  [`calculate_psv`](https://peteowen1.github.io/panna/reference/calculate_psv.md).

## Value

A data.table with `psv`, `osv`, `dsv` columns.
