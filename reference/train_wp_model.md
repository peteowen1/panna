# Train a win probability model

Fits an XGBoost model to predict match outcome (home win / draw / away
win) from in-match game state features. Uses `WP_DRAW_VALUE` (0.5) for
draws so the model predicts "expected points fraction" for the home
team.

## Usage

``` r
train_wp_model(wp_features, nrounds = 200L, max_depth = 4L, eta = 0.05, ...)
```

## Arguments

- wp_features:

  Output of
  [`create_wp_features`](https://peteowen1.github.io/panna/reference/create_wp_features.md)
  with `wp_label` column.

- nrounds:

  Number of XGBoost boosting rounds (default 200).

- max_depth:

  Maximum tree depth (default 4).

- eta:

  Learning rate (default 0.05).

- ...:

  Additional parameters passed to
  [`xgboost::xgb.train()`](https://rdrr.io/pkg/xgboost/man/xgb.train.html).

## Value

A list with:

- model:

  Trained xgboost model object

- feature_names:

  Character vector of feature column names
