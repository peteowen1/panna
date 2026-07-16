# Extract RAPM ratings from fitted model

Calculates player ratings as offense_coef - defense_coef. Positive =
good, negative = bad.

## Usage

``` r
extract_rapm_ratings(model, lambda = "min")
```

## Arguments

- model:

  Fitted RAPM model from fit_rapm

- lambda:

  Which lambda to use ("min" or "1se")

## Value

Data frame with player ratings

## See also

Other rapm:
[`create_all_splints()`](https://peteowen1.github.io/panna/reference/create_all_splints.md),
[`create_rapm_design_matrix()`](https://peteowen1.github.io/panna/reference/create_rapm_design_matrix.md),
[`extract_period_end_times()`](https://peteowen1.github.io/panna/reference/extract_period_end_times.md),
[`extract_player_timing_from_events()`](https://peteowen1.github.io/panna/reference/extract_player_timing_from_events.md),
[`fit_rapm()`](https://peteowen1.github.io/panna/reference/fit_rapm.md),
[`fit_rapm_with_prior()`](https://peteowen1.github.io/panna/reference/fit_rapm_with_prior.md)
