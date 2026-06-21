# Calculate Per-Game Player Stat Value

Applies pre-trained glmnet coefficients to raw per-game box-score stats,
producing a single-game "stat contribution" value. This is the per-game
analogue of
[`calculate_psr`](https://peteowen1.github.io/panna/reference/calculate_psr.md),
which operates on smoothed skill ratings.

## Usage

``` r
calculate_psv(
  player_match_stats,
  coef_df,
  min_adjust = TRUE,
  center = TRUE,
  exclude_efficiency = TRUE,
  scale_to_minutes = FALSE
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

- min_adjust:

  Logical. Divide raw counts by `minutes_played / 90` to get per-90
  rates before applying coefficients. Default `TRUE`.

- center:

  Logical. Center PSV within each matchday/round so PSV = contribution
  above average that round. Default `TRUE`.

- exclude_efficiency:

  Logical. Exclude efficiency/ratio stats from PSV calculation. Default
  `TRUE`.

- scale_to_minutes:

  Logical. If TRUE, multiply the (per-90) PSV by `minutes_played / 90`
  so the result is additive over a player's games (like EPV), rather
  than a per-90 rate. Default `FALSE` (per-90, the form consumed by the
  multi-target RAPM and skills pipeline).

## Value

A data.table with identifier columns plus `psv_raw` and `psv`.

## Details

Stats are minutes-adjusted (divided by `minutes_played / 90`) to get
per-90 rates, then optionally standardized using training SDs from the
coefficient file. Efficiency stats (ratios) are excluded from PSV by
default.
