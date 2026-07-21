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
  scale_to_minutes = FALSE,
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

- reliability:

  Optional data.frame with columns `stat_name` and `lambda` (see
  [`load_psv_match_reliability`](https://peteowen1.github.io/panna/reference/load_psv_match_reliability.md)),
  pre-filtered to a single population (`compute_player_psv` does this
  via `model`). When supplied, each standardized stat column is
  multiplied by that stat's `lambda` in `[0, 1]` – the reliability of a
  SINGLE match as evidence of persistent player skill
  (`Var_between / (Var_between + Var_within)` from a variance
  decomposition over players). Standardization always uses the
  coefficient file's `sd` (the scale betas are calibrated to); a v1
  design that instead swapped the standardization denominator for a
  per-match sd was rejected by the empirical gate (it re-weighted
  features by `sd_train/sd_match`, up to 38x, which AMPLIFIES rather
  than damps stable-scale features). Because `lambda <= 1`, reliability
  shrinkage can only shrink a contribution, never amplify it. A stat
  present in `coef_df` but absent from `reliability` (or with an `NA`
  lambda, e.g. too few players to estimate) is left unshrunk
  (`lambda = 1`) with a
  [`cli::cli_warn`](https://cli.r-lib.org/reference/cli_abort.html)
  naming it. Default `NULL` (no shrinkage, unchanged behaviour). When
  supplied (non-NULL, non-empty), `psv_raw`/`psv` are ALSO multiplied by
  [`PSV_RELIABILITY_GD_SCALE`](https://peteowen1.github.io/panna/reference/PSV_RELIABILITY_GD_SCALE.md),
  putting the result in "expected GD contribution per 90" units (see
  that constant's docs for the derivation) – the `reliability = NULL`
  path is unaffected and stays bit-identical to the pre-scale behaviour.

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

A data.table with identifier columns plus `psv_raw` and `psv`.

## Details

Stats are minutes-adjusted (divided by `minutes_played / 90`) to get
per-90 rates, then optionally standardized using training SDs from the
coefficient file. Efficiency stats (ratios) are excluded from PSV by
default.

## See also

Other psr:
[`PSV_RELIABILITY_GD_SCALE`](https://peteowen1.github.io/panna/reference/PSV_RELIABILITY_GD_SCALE.md),
[`calculate_psr()`](https://peteowen1.github.io/panna/reference/calculate_psr.md),
[`calculate_psv_components()`](https://peteowen1.github.io/panna/reference/calculate_psv_components.md),
[`compute_player_psv()`](https://peteowen1.github.io/panna/reference/compute_player_psv.md),
[`default_stat_rating_params()`](https://peteowen1.github.io/panna/reference/default_stat_rating_params.md),
[`load_opta_psr_weekly()`](https://peteowen1.github.io/panna/reference/load_opta_psr_weekly.md),
[`load_psv_match_reliability()`](https://peteowen1.github.io/panna/reference/load_psv_match_reliability.md),
[`player_psr()`](https://peteowen1.github.io/panna/reference/player_psr.md),
[`soccer_position_map()`](https://peteowen1.github.io/panna/reference/soccer_position_map.md),
[`soccer_stat_rating_definitions()`](https://peteowen1.github.io/panna/reference/soccer_stat_rating_definitions.md),
[`stat_rating_names()`](https://peteowen1.github.io/panna/reference/stat_rating_names.md)
