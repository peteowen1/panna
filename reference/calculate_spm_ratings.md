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
