# Compare SPM feature importance between two models

Compares which features are most important between two fitted SPM models
(e.g. different seasons, targets, or feature sets). Useful for
understanding which features drive each model's ratings.

## Usage

``` r
compare_spm_features(fbref_model, opta_model, n = 20)
```

## Arguments

- fbref_model:

  Fitted SPM model to compare (labeled "FBref" in the output's `source`
  column for historical reasons; any fitted SPM model works)

- opta_model:

  Fitted SPM model to compare against (labeled "Opta" in the output's
  `source` column)

- n:

  Number of top features to compare (default 20)

## Value

Data frame comparing feature importance
