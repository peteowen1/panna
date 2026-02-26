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
