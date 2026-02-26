# Prepare SPM regression data

Joins player features with RAPM ratings for SPM model fitting.

## Usage

``` r
prepare_spm_regression_data(player_features, rapm_ratings)
```

## Arguments

- player_features:

  Data frame from create_player_feature_matrix

- rapm_ratings:

  Data frame from extract_rapm_coefficients

## Value

Data frame ready for SPM regression
