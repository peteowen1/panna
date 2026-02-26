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
