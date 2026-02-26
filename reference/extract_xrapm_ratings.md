# Extract xRAPM ratings (with prior)

Extracts player ratings from a model fit with SPM prior. The final
coefficient is gamma + prior, where gamma is the deviation.

## Usage

``` r
extract_xrapm_ratings(model, lambda = "min")
```

## Arguments

- model:

  Fitted xRAPM model from fit_rapm_with_prior

- lambda:

  Which lambda to use ("min" or "1se")

## Value

Data frame with player ratings including deviation from prior
