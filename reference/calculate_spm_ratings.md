# Calculate SPM ratings for all players

Applies SPM model to predict RAPM for all players with features.

## Usage

``` r
calculate_spm_ratings(player_features, spm_model, lambda = "min")
```

## Arguments

- player_features:

  Data frame of player features

- spm_model:

  Fitted SPM model

- lambda:

  Which lambda to use

## Value

Data frame with SPM ratings

## See also

Other spm:
[`calculate_spm_ratings_xgb()`](https://peteowen1.github.io/panna/reference/calculate_spm_ratings_xgb.md),
[`fit_spm_model()`](https://peteowen1.github.io/panna/reference/fit_spm_model.md),
[`get_spm_feature_importance()`](https://peteowen1.github.io/panna/reference/get_spm_feature_importance.md)
