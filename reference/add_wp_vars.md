# Add win probability and WPA to SPADL data

Adds `wp` (win probability at each action from home perspective) and
`wpa` (win probability added by each action, sign-adjusted for the
acting team) columns.

## Usage

``` r
add_wp_vars(wp_features, wp_model)
```

## Arguments

- wp_features:

  SPADL features with WP model features.

- wp_model:

  Trained WP model from
  [`train_wp_model`](https://peteowen1.github.io/panna/reference/train_wp_model.md).

## Value

The input data.table with added `wp` and `wpa` columns.

## Details

WPA is computed as the change in win probability caused by each action.
For home team actions: `wpa = wp_after - wp_before`. For away team
actions:
`wpa = (1 - wp_after) - (1 - wp_before) = wp_before - wp_after`.
