# Compare FBref and Opta SPM feature importance

Compares which features are most important in FBref vs Opta SPM models.
Useful for understanding which data source captures different aspects of
play.

## Usage

``` r
compare_spm_features(fbref_model, opta_model, n = 20)
```

## Arguments

- fbref_model:

  Fitted SPM model from FBref data

- opta_model:

  Fitted SPM model from Opta data

- n:

  Number of top features to compare (default 20)

## Value

Data frame comparing feature importance
