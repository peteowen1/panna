# Calculate Player Skill Ratings (PSR)

Computes PSR for each player by applying pre-trained glmnet coefficients
to individual player skill values. PSR represents each player's
predicted contribution to xG/goal differential based on their skill
profile.

## Usage

``` r
calculate_psr(skills, coef_df, center = TRUE)
```

## Arguments

- skills:

  A data.table/data.frame with player skill estimates, containing
  identity columns (`player_id`, `player_name`) and numeric skill
  columns matching the `stat_name` values in `coef_df`.

- coef_df:

  A data.frame with columns `stat_name` and `beta`. If an `sd` column is
  present, each skill is divided by its SD before multiplying by beta
  (i.e. the coefficients are on the standardized scale).

- center:

  Logical. If TRUE (default), subtract the league mean so PSR =
  contribution above average player.

## Value

A data.table with identity columns plus `psr_raw` and `psr`.

## See also

Other psr:
[`PSV_RELIABILITY_GD_SCALE`](https://peteowen1.github.io/panna/reference/PSV_RELIABILITY_GD_SCALE.md),
[`calculate_psv()`](https://peteowen1.github.io/panna/reference/calculate_psv.md),
[`calculate_psv_components()`](https://peteowen1.github.io/panna/reference/calculate_psv_components.md),
[`compute_player_psv()`](https://peteowen1.github.io/panna/reference/compute_player_psv.md),
[`default_stat_rating_params()`](https://peteowen1.github.io/panna/reference/default_stat_rating_params.md),
[`load_opta_psr_weekly()`](https://peteowen1.github.io/panna/reference/load_opta_psr_weekly.md),
[`load_psv_match_reliability()`](https://peteowen1.github.io/panna/reference/load_psv_match_reliability.md),
[`player_psr()`](https://peteowen1.github.io/panna/reference/player_psr.md),
[`soccer_position_map()`](https://peteowen1.github.io/panna/reference/soccer_position_map.md),
[`soccer_stat_rating_definitions()`](https://peteowen1.github.io/panna/reference/soccer_stat_rating_definitions.md),
[`stat_rating_names()`](https://peteowen1.github.io/panna/reference/stat_rating_names.md)
