# Validate predictive power

Tests panna ratings against next season performance.

## Usage

``` r
validate_predictive_power(panna_ratings, next_season_rapm)
```

## Arguments

- panna_ratings:

  Panna ratings from season N

- next_season_rapm:

  RAPM from season N+1

## Value

Validation metrics

## Examples

``` r
if (FALSE) { # \dontrun{
metrics <- validate_predictive_power(panna_ratings_2023, rapm_ratings_2024)
metrics$correlation
metrics$rmse
} # }
```
