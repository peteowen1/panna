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
  exclude_efficiency = TRUE,
  reliability = NULL,
  center_weights = c("none", "minutes")
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

- reliability:

  Optional per-match reliability lookup, passed through to each of the
  three
  [`calculate_psv`](https://peteowen1.github.io/panna/reference/calculate_psv.md)
  calls (margin/offense/defense) so all three components shrink stats on
  the same scale. See
  [`calculate_psv`](https://peteowen1.github.io/panna/reference/calculate_psv.md).

- center_weights:

  One of `"none"` (default) or `"minutes"`. `"none"` centers on the
  plain row mean of `psv_raw` within each `(season, round)` group –
  unchanged legacy behaviour, and the ONLY path the RAPM `psvf90` target
  and every other pre-existing caller use (bit-identical when this
  argument is left at its default). `"minutes"` centers on the
  minutes-weighted mean instead (weight = `minutes_played / 90`, or
  `total_minutes / 90` when that's the only minutes column present – the
  same resolution `scale_to_minutes` uses). Combined with
  `scale_to_minutes = TRUE` this makes the round's SUMMED
  (minutes-scaled) PSV exactly 0: writing \\w_i = minutes_i/90\\ and
  \\\bar{x}\_w = \sum w_i x_i / \sum w_i\\ for the weighted mean of
  `psv_raw`, the scaled centered value is \\w_i(x_i - \bar{x}\_w)\\, and
  \\\sum_i w_i(x_i - \bar{x}\_w) = \sum_i w_i x_i - \bar{x}\_w \sum_i
  w_i = 0\\ by construction. A group whose weights all resolve to 0
  (e.g. every row's minutes missing/non-positive) falls back to the
  plain mean for that group so centering never divides by zero. Display
  path only (game-logs export) – has no effect when `center = FALSE`.

## Value

A data.table with identifier columns plus `psv_raw`, `psv`, `osv`,
`dsv`.

## See also

Other psr:
[`PSV_RELIABILITY_GD_SCALE`](https://peteowen1.github.io/panna/reference/PSV_RELIABILITY_GD_SCALE.md),
[`calculate_psr()`](https://peteowen1.github.io/panna/reference/calculate_psr.md),
[`calculate_psv()`](https://peteowen1.github.io/panna/reference/calculate_psv.md),
[`compute_player_psv()`](https://peteowen1.github.io/panna/reference/compute_player_psv.md),
[`default_stat_rating_params()`](https://peteowen1.github.io/panna/reference/default_stat_rating_params.md),
[`load_opta_psr_weekly()`](https://peteowen1.github.io/panna/reference/load_opta_psr_weekly.md),
[`load_psv_match_reliability()`](https://peteowen1.github.io/panna/reference/load_psv_match_reliability.md),
[`player_psr()`](https://peteowen1.github.io/panna/reference/player_psr.md),
[`soccer_position_map()`](https://peteowen1.github.io/panna/reference/soccer_position_map.md),
[`soccer_stat_rating_definitions()`](https://peteowen1.github.io/panna/reference/soccer_stat_rating_definitions.md),
[`stat_rating_names()`](https://peteowen1.github.io/panna/reference/stat_rating_names.md)
