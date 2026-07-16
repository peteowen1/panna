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

## See also

Other win probability:
[`add_wp_vars()`](https://peteowen1.github.io/panna/reference/add_wp_vars.md),
[`aggregate_player_game_wpa()`](https://peteowen1.github.io/panna/reference/aggregate_player_game_wpa.md),
[`assign_wpa_credit()`](https://peteowen1.github.io/panna/reference/assign_wpa_credit.md),
[`create_wp_features()`](https://peteowen1.github.io/panna/reference/create_wp_features.md),
[`load_wp_model()`](https://peteowen1.github.io/panna/reference/load_wp_model.md),
[`save_wp_model()`](https://peteowen1.github.io/panna/reference/save_wp_model.md),
[`train_wp_model()`](https://peteowen1.github.io/panna/reference/train_wp_model.md)
