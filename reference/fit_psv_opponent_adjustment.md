# Fit the PSV opponent-adjustment coefficient

Regresses per-90 PSV on the opponent's defensive rating across
player-matches and returns the slope. Fitted on **starters only** and on
a leak-free pairing (season S-1 team strength against season S matches)
for the same reasons the position factors are: substitution is
endogenous to match state, and a same-season team rating contains the
matches being explained.

## Usage

``` r
fit_psv_opponent_adjustment(
  player_match,
  min_minutes = 45,
  starters_only = TRUE
)
```

## Arguments

- player_match:

  A data.frame of player-match rows carrying `psv`, `opp_def_rating`,
  and a minutes column.

- min_minutes:

  Minimum minutes for a row to count. Default 45.

- starters_only:

  Drop `position == "Substitute"` rows. Default TRUE.

## Value

A list with `gamma`, `se`, `n_obs` and `r_squared`.

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
[`load_opta_psr_weekly()`](https://peteowen1.github.io/panna/reference/load_opta_psr_weekly.md),
[`load_psr_calibration()`](https://peteowen1.github.io/panna/reference/load_psr_calibration.md),
[`load_psv_match_reliability()`](https://peteowen1.github.io/panna/reference/load_psv_match_reliability.md),
[`player_psr()`](https://peteowen1.github.io/panna/reference/player_psr.md),
[`psr_leaderboard_eligible()`](https://peteowen1.github.io/panna/reference/psr_leaderboard_eligible.md),
[`psv_opponent`](https://peteowen1.github.io/panna/reference/psv_opponent.md),
[`soccer_position_map()`](https://peteowen1.github.io/panna/reference/soccer_position_map.md),
[`soccer_stat_rating_definitions()`](https://peteowen1.github.io/panna/reference/soccer_stat_rating_definitions.md),
[`stat_rating_names()`](https://peteowen1.github.io/panna/reference/stat_rating_names.md)
