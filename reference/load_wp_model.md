# Load WP model

Resolution order, matching load_epv_model() / load_xpass_model():

1.  Explicit `path` (if supplied and the file exists)

2.  pannamodels package (preferred — distributes wp_model via the `epv`
    release tag, downloaded + cached on first call)

3.  Local fallback at `pannadata/data/opta/models/wp_model.rds`

## Usage

``` r
load_wp_model(path = NULL)
```

## Arguments

- path:

  Directory to load from. If NULL, tries pannamodels first then falls
  back to `pannadata/data/opta/models/`.

## Value

WP model list (model + feature_names).

## See also

Other win probability:
[`add_wp_vars()`](https://peteowen1.github.io/panna/reference/add_wp_vars.md),
[`aggregate_player_game_wpa()`](https://peteowen1.github.io/panna/reference/aggregate_player_game_wpa.md),
[`assign_wpa_credit()`](https://peteowen1.github.io/panna/reference/assign_wpa_credit.md),
[`create_wp_features()`](https://peteowen1.github.io/panna/reference/create_wp_features.md),
[`predict_wp()`](https://peteowen1.github.io/panna/reference/predict_wp.md),
[`save_wp_model()`](https://peteowen1.github.io/panna/reference/save_wp_model.md),
[`train_wp_model()`](https://peteowen1.github.io/panna/reference/train_wp_model.md)
