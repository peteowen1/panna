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
  center = TRUE,
  scale_to_minutes = FALSE,
  exclude_efficiency = TRUE
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

- scale_to_minutes:

  Logical. If TRUE, multiply the (per-90) PSV by `minutes_played / 90`
  so the result is additive over a player's games (like EPV), rather
  than a per-90 rate. Default `FALSE` (per-90, the form consumed by the
  multi-target RAPM and skills pipeline).

- exclude_efficiency:

  Logical. Exclude efficiency/ratio stats from PSV calculation. Default
  `TRUE`.

## Value

A data.table with identifier columns plus `psv_raw`, `psv`, `osv`,
`dsv`.

## See also

Other psr:
[`calculate_psr()`](https://peteowen1.github.io/panna/reference/calculate_psr.md),
[`calculate_psv()`](https://peteowen1.github.io/panna/reference/calculate_psv.md),
[`compute_player_psv()`](https://peteowen1.github.io/panna/reference/compute_player_psv.md),
[`default_stat_rating_params()`](https://peteowen1.github.io/panna/reference/default_stat_rating_params.md),
[`load_opta_psr_weekly()`](https://peteowen1.github.io/panna/reference/load_opta_psr_weekly.md),
[`player_psr()`](https://peteowen1.github.io/panna/reference/player_psr.md),
[`soccer_position_map()`](https://peteowen1.github.io/panna/reference/soccer_position_map.md),
[`soccer_stat_rating_definitions()`](https://peteowen1.github.io/panna/reference/soccer_stat_rating_definitions.md),
[`stat_rating_names()`](https://peteowen1.github.io/panna/reference/stat_rating_names.md)
