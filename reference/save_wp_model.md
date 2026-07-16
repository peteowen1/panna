# Save WP model

Save WP model

## Usage

``` r
save_wp_model(wp_model, path = NULL)
```

## Arguments

- wp_model:

  WP model from
  [`train_wp_model`](https://peteowen1.github.io/panna/reference/train_wp_model.md).

- path:

  Directory to save. If NULL, uses `pannadata/data/opta/models/`.

## Value

Invisibly returns the file path.

## See also

Other win probability:
[`add_wp_vars()`](https://peteowen1.github.io/panna/reference/add_wp_vars.md),
[`aggregate_player_game_wpa()`](https://peteowen1.github.io/panna/reference/aggregate_player_game_wpa.md),
[`assign_wpa_credit()`](https://peteowen1.github.io/panna/reference/assign_wpa_credit.md),
[`create_wp_features()`](https://peteowen1.github.io/panna/reference/create_wp_features.md),
[`load_wp_model()`](https://peteowen1.github.io/panna/reference/load_wp_model.md),
[`predict_wp()`](https://peteowen1.github.io/panna/reference/predict_wp.md),
[`train_wp_model()`](https://peteowen1.github.io/panna/reference/train_wp_model.md)
