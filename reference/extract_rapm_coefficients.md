# Extract RAPM coefficients

Gets player ratings from a fitted RAPM model.

## Usage

``` r
extract_rapm_coefficients(model, lambda = "min")
```

## Arguments

- model:

  Fitted RAPM model from fit_rapm

- lambda:

  Which lambda to use ("min" or "1se" or numeric)

## Value

Data frame with player ratings
