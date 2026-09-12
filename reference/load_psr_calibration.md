# Load the PSR calibration table

Per-position and per-season multipliers putting PSR on a common
goals-per-90 footing (panna#202/#213/#214). See
`inst/extdata/psr_calibration.csv`.

## Usage

``` r
load_psr_calibration()
```

## Value

data.table with `axis` ("position"/"season"), `level`, `factor`, plus
the underlying `slope`/`se` for auditing.

## Details

Derived leak-free: each player's season S-1 rating predicts season S
matches, minute-weighted and summed per position group, entered as
own-minus-opponent differences against actual goal difference. The
fitted slope for a cell is how much goal difference one unit of its
rating actually buys, so multiplying by that slope maps the cell into
goal units.

Estimated as two SEPARABLE marginals, not 6 x 13 joint cells: each
marginal is well powered, the joint would be mostly sampling noise.

**The factor IS the slope**, so calibrated PSR is denominated in goals:
if `GD = slope * psr`, then `psr * slope` regresses on goal difference
with a coefficient of 1. Verified after application – every position
lands at 0.97-1.02 and every season at 0.98-1.03 (sd around 1 of 0.015).
A player with calibrated PSR 0.30 contributed about 0.30 goals of
difference per 90, whatever their position or era.

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
[`load_psv_match_reliability()`](https://peteowen1.github.io/panna/reference/load_psv_match_reliability.md),
[`player_psr()`](https://peteowen1.github.io/panna/reference/player_psr.md),
[`psr_leaderboard_eligible()`](https://peteowen1.github.io/panna/reference/psr_leaderboard_eligible.md),
[`psv_opponent`](https://peteowen1.github.io/panna/reference/psv_opponent.md),
[`soccer_position_map()`](https://peteowen1.github.io/panna/reference/soccer_position_map.md),
[`soccer_stat_rating_definitions()`](https://peteowen1.github.io/panna/reference/soccer_stat_rating_definitions.md),
[`stat_rating_names()`](https://peteowen1.github.io/panna/reference/stat_rating_names.md)
