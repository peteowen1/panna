# Predict win probability

Scores each action's game state with win probability using a trained
model.

## Usage

``` r
predict_wp(wp_model, wp_features)
```

## Arguments

- wp_model:

  Output of
  [`train_wp_model`](https://peteowen1.github.io/panna/reference/train_wp_model.md).

- wp_features:

  SPADL features from
  [`create_wp_features`](https://peteowen1.github.io/panna/reference/create_wp_features.md).

## Value

Numeric vector of win probabilities (home team perspective), same length
as `nrow(wp_features)`.
