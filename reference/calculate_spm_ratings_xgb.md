# Calculate SPM ratings using XGBoost model

Calculate SPM ratings using XGBoost model

## Usage

``` r
calculate_spm_ratings_xgb(player_features, spm_xgb_model)
```

## Arguments

- player_features:

  Data frame of player features

- spm_xgb_model:

  Fitted XGBoost SPM model from fit_spm_xgb

## Value

Data frame with SPM ratings

## See also

Other spm:
[`calculate_spm_ratings()`](https://peteowen1.github.io/panna/reference/calculate_spm_ratings.md),
[`fit_spm_model()`](https://peteowen1.github.io/panna/reference/fit_spm_model.md),
[`get_spm_feature_importance()`](https://peteowen1.github.io/panna/reference/get_spm_feature_importance.md)
