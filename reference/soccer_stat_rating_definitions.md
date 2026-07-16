# Soccer stat rating definitions

Returns a data.frame describing all stats used in the skill estimation
and PSR/PSV pipelines. Each stat has a type (rate or efficiency),
category (offensive, defensive, goalkeeper, xmetrics), and metadata
about adjustment.

## Usage

``` r
soccer_stat_rating_definitions()
```

## Value

A data.frame with columns:

- stat_name:

  Column name in match stats

- type:

  "rate" (Gamma-Poisson) or "efficiency" (Beta-Binomial)

- category:

  "offensive", "defensive", "goalkeeper", "xmetrics", or "general"

- pos_adjusted:

  Logical; TRUE if prior is position-specific

## See also

Other psr:
[`calculate_psr()`](https://peteowen1.github.io/panna/reference/calculate_psr.md),
[`calculate_psv()`](https://peteowen1.github.io/panna/reference/calculate_psv.md),
[`calculate_psv_components()`](https://peteowen1.github.io/panna/reference/calculate_psv_components.md),
[`compute_player_psv()`](https://peteowen1.github.io/panna/reference/compute_player_psv.md),
[`default_stat_rating_params()`](https://peteowen1.github.io/panna/reference/default_stat_rating_params.md),
[`load_opta_psr_weekly()`](https://peteowen1.github.io/panna/reference/load_opta_psr_weekly.md),
[`player_psr()`](https://peteowen1.github.io/panna/reference/player_psr.md),
[`soccer_position_map()`](https://peteowen1.github.io/panna/reference/soccer_position_map.md),
[`stat_rating_names()`](https://peteowen1.github.io/panna/reference/stat_rating_names.md)
