# Get all stat rating column names

Returns the stat names from
[`soccer_stat_rating_definitions`](https://peteowen1.github.io/panna/reference/soccer_stat_rating_definitions.md),
optionally filtered by type or category.

## Usage

``` r
stat_rating_names(type = NULL, category = NULL)
```

## Arguments

- type:

  Filter: "rate", "efficiency", or NULL for all.

- category:

  Filter: "offensive", "defensive", "goalkeeper", "xmetrics", "general",
  or NULL for all.

## Value

Character vector of stat names.

## See also

Other psr:
[`PSV_RELIABILITY_GD_SCALE`](https://peteowen1.github.io/panna/reference/PSV_RELIABILITY_GD_SCALE.md),
[`apply_psr_calibration()`](https://peteowen1.github.io/panna/reference/apply_psr_calibration.md),
[`apply_psr_season_calibration()`](https://peteowen1.github.io/panna/reference/apply_psr_season_calibration.md),
[`apply_psv_calibration()`](https://peteowen1.github.io/panna/reference/apply_psv_calibration.md),
[`apply_psv_opponent_adjustment()`](https://peteowen1.github.io/panna/reference/apply_psv_opponent_adjustment.md),
[`calculate_psr()`](https://peteowen1.github.io/panna/reference/calculate_psr.md),
[`calculate_psv()`](https://peteowen1.github.io/panna/reference/calculate_psv.md),
[`calculate_psv_components()`](https://peteowen1.github.io/panna/reference/calculate_psv_components.md),
[`compute_player_psv()`](https://peteowen1.github.io/panna/reference/compute_player_psv.md),
[`default_stat_rating_params()`](https://peteowen1.github.io/panna/reference/default_stat_rating_params.md),
[`fit_psv_opponent_adjustment()`](https://peteowen1.github.io/panna/reference/fit_psv_opponent_adjustment.md),
[`load_opta_psr_weekly()`](https://peteowen1.github.io/panna/reference/load_opta_psr_weekly.md),
[`load_psr_calibration()`](https://peteowen1.github.io/panna/reference/load_psr_calibration.md),
[`load_psv_match_reliability()`](https://peteowen1.github.io/panna/reference/load_psv_match_reliability.md),
[`player_psr()`](https://peteowen1.github.io/panna/reference/player_psr.md),
[`psr_leaderboard_eligible()`](https://peteowen1.github.io/panna/reference/psr_leaderboard_eligible.md),
[`psv_opponent`](https://peteowen1.github.io/panna/reference/psv_opponent.md),
[`soccer_position_map()`](https://peteowen1.github.io/panna/reference/soccer_position_map.md),
[`soccer_stat_rating_definitions()`](https://peteowen1.github.io/panna/reference/soccer_stat_rating_definitions.md)
