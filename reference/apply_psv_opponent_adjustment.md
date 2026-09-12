# Apply the PSV opponent adjustment

Subtracts `gamma * opp_def_rating` from `psv`, preserving the
`osv + dsv == psv` identity by splitting the adjustment evenly across
the two components when they are present – the same convention
`10b_export_game_logs.R` uses for the league offset.

## Usage

``` r
apply_psv_opponent_adjustment(game_logs, gamma, verbose = FALSE)
```

## Arguments

- game_logs:

  A data.frame with `psv` and `opp_def_rating`.

- gamma:

  The coefficient from
  [`fit_psv_opponent_adjustment()`](https://peteowen1.github.io/panna/reference/fit_psv_opponent_adjustment.md).

- verbose:

  Print how many rows were adjusted.

## Value

`game_logs` with `psv` adjusted and `psv_opp_adjustment` added.

## Details

Rows with no `opp_def_rating` are left **unadjusted** rather than
filled. A constant fill is what made the opponent control inert in eight
competitions (panna#224); leaving a row alone is honest and visible in
the returned count.

## See also

Other psr:
[`PSV_RELIABILITY_GD_SCALE`](https://peteowen1.github.io/panna/reference/PSV_RELIABILITY_GD_SCALE.md),
[`apply_psr_calibration()`](https://peteowen1.github.io/panna/reference/apply_psr_calibration.md),
[`apply_psr_season_calibration()`](https://peteowen1.github.io/panna/reference/apply_psr_season_calibration.md),
[`apply_psv_calibration()`](https://peteowen1.github.io/panna/reference/apply_psv_calibration.md),
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
[`soccer_stat_rating_definitions()`](https://peteowen1.github.io/panna/reference/soccer_stat_rating_definitions.md),
[`stat_rating_names()`](https://peteowen1.github.io/panna/reference/stat_rating_names.md)
