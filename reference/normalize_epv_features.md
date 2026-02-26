# Normalize Features for Model

Applies standardization to numeric features.

## Usage

``` r
normalize_epv_features(features, feature_cols = NULL, means = NULL, sds = NULL)
```

## Arguments

- features:

  Feature data frame

- feature_cols:

  Columns to normalize

- means:

  Optional pre-computed means

- sds:

  Optional pre-computed standard deviations

## Value

List with normalized features, means, and sds
