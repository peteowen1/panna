# Calculate PSV with Offensive/Defensive Decomposition

Applies offensive and defensive coefficient models to per-game stats,
producing `psv`, `osv`, and `dsv` columns where `osv + dsv = psv`
exactly (via additive reconciliation).

## Usage

``` r
calculate_psv_components(
  player_match_stats,
  coef_df,
  osr_coef_df,
  dsr_coef_df,
  min_adjust = TRUE,
  center = TRUE
)
```

## Arguments

- player_match_stats:

  Data.frame/data.table with one row per player per match. Must contain
  raw stat columns matching `coef_df$stat_name`, plus optionally
  `minutes_played` (or `total_minutes`).

- coef_df:

  Coefficient data.frame with columns `stat_name`, `beta`, and
  optionally `sd`.

- osr_coef_df:

  Coefficient data.frame for the offensive model (predicting goals
  scored / xG for).

- dsr_coef_df:

  Coefficient data.frame for the defensive model (predicting goals
  conceded / xG against).

- min_adjust:

  Logical. Divide raw counts by `minutes_played / 90` to get per-90
  rates before applying coefficients. Default `TRUE`.

- center:

  Logical. Center PSV within each matchday/round so PSV = contribution
  above average that round. Default `TRUE`.

## Value

A data.table with identifier columns plus `psv_raw`, `psv`, `osv`,
`dsv`.
