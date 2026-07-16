# Get top SPM feature importance

Identifies the most important features in the SPM model.

## Usage

``` r
get_spm_feature_importance(model, n = 10, lambda = "min")
```

## Arguments

- model:

  Fitted SPM model

- n:

  Number of top features to return

- lambda:

  Which lambda to use

## Value

Data frame of top features by absolute coefficient

## See also

Other spm:
[`calculate_spm_ratings()`](https://peteowen1.github.io/panna/reference/calculate_spm_ratings.md),
[`calculate_spm_ratings_xgb()`](https://peteowen1.github.io/panna/reference/calculate_spm_ratings_xgb.md),
[`fit_spm_model()`](https://peteowen1.github.io/panna/reference/fit_spm_model.md)
