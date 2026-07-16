# Default stat rating hyperparameters

Returns default decay rates and prior strengths for the soccer stat
rating estimation pipeline. Can be customized per-stat via named
overrides.

## Usage

``` r
default_stat_rating_params()
```

## Value

A list with elements:

- rate:

  Decay lambda for rate (per-90) stats, default 0.003 (~231 day
  half-life)

- efficiency:

  Decay lambda for efficiency stats, default 0.002 (~347 day half-life)

- xmetrics:

  Decay lambda for xMetrics stats, default 0.003

- prior_90s:

  Gamma prior strength in equivalent 90-minute matches, default 2

- prior_attempts:

  Beta prior strength in equivalent attempts, default 50

## See also

Other psr:
[`calculate_psr()`](https://peteowen1.github.io/panna/reference/calculate_psr.md),
[`calculate_psv()`](https://peteowen1.github.io/panna/reference/calculate_psv.md),
[`calculate_psv_components()`](https://peteowen1.github.io/panna/reference/calculate_psv_components.md),
[`compute_player_psv()`](https://peteowen1.github.io/panna/reference/compute_player_psv.md),
[`load_opta_psr_weekly()`](https://peteowen1.github.io/panna/reference/load_opta_psr_weekly.md),
[`player_psr()`](https://peteowen1.github.io/panna/reference/player_psr.md),
[`soccer_position_map()`](https://peteowen1.github.io/panna/reference/soccer_position_map.md),
[`soccer_stat_rating_definitions()`](https://peteowen1.github.io/panna/reference/soccer_stat_rating_definitions.md),
[`stat_rating_names()`](https://peteowen1.github.io/panna/reference/stat_rating_names.md)
