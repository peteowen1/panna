# Apply the PSV position calibration

Multiplies `psv` (and `osv`/`dsv` when present, so `osv + dsv == psv` is
preserved) by that player's position factor.

## Usage

``` r
apply_psv_calibration(
  psv_dt,
  position_col = "pos_grp",
  calibration = load_psv_calibration()
)
```

## Arguments

- psv_dt:

  A data.frame/data.table with `psv` and a position column.

- position_col:

  Name of the resolved-position column. Default `"pos_grp"`;
  `"primary_position"` and `"position"` are used as fallbacks when
  present.

- calibration:

  Calibration table; defaults to
  [`load_psv_calibration`](https://peteowen1.github.io/panna/reference/load_psv_calibration.md).

## Value

`psv_dt` with `psv` (and `osv`/`dsv`) scaled, and an attribute
`panna_psv_calibrated = TRUE`.

## Details

## See also

[`load_psv_calibration`](https://peteowen1.github.io/panna/reference/load_psv_calibration.md),
[`compute_psr_league_offsets`](https://peteowen1.github.io/panna/reference/compute_psr_league_offsets.md)

Other psr:
[`PSV_RELIABILITY_GD_SCALE`](https://peteowen1.github.io/panna/reference/PSV_RELIABILITY_GD_SCALE.md),
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
[`psv_opponent`](https://peteowen1.github.io/panna/reference/psv_opponent.md),
[`soccer_position_map()`](https://peteowen1.github.io/panna/reference/soccer_position_map.md),
[`soccer_stat_rating_definitions()`](https://peteowen1.github.io/panna/reference/soccer_stat_rating_definitions.md),
[`stat_rating_names()`](https://peteowen1.github.io/panna/reference/stat_rating_names.md)
