# Train a win probability model

Fits an XGBoost model to predict match outcome (home win / draw / away
win) from in-match game state features. Uses `WP_DRAW_VALUE` (0.5) for
draws so the model predicts home's expected points fraction. Uses
binary:logistic (cross-entropy with fractional labels) for consistency
with torp's AFL WP training harness and natural `[0,1]` output via
sigmoid.

## Usage

``` r
train_wp_model(
  wp_features,
  nrounds = 500L,
  max_depth = 4L,
  eta = 0.05,
  nfolds = 5L,
  min_child_weight = 50L,
  feature_names = NULL,
  objective = "binary:logistic",
  early_stopping_rounds = 20L,
  seed = 42L,
  ...
)
```

## Arguments

- wp_features:

  Output of
  [`create_wp_features`](https://peteowen1.github.io/panna/reference/create_wp_features.md)
  with `wp_label` column and `match_id` (for group-aware folds).

- nrounds:

  Maximum boosting rounds (default 500; early stopping typically halts
  well before).

- max_depth:

  Maximum tree depth (default 4).

- eta:

  Learning rate (default 0.05).

- nfolds:

  Number of CV folds (default 5).

- early_stopping_rounds:

  Stop CV if logloss hasn't improved in this many rounds (default 20).

- seed:

  Random seed for reproducibility (default 42).

- ...:

  Additional parameters passed to
  [`xgboost::xgb.train()`](https://rdrr.io/pkg/xgboost/man/xgb.train.html).

## Value

A list with:

- model:

  Trained xgboost model object

- feature_names:

  Character vector of feature column names

- cv_logloss:

  Best CV logloss (held-out mean)

- optimal_nrounds:

  The nrounds selected by early stopping

## Details

Two-step training (matches torp::train_live_wp_xgb.R):

1.  5-fold match-grouped xgb.cv with early_stopping_rounds=20 to find
    optimal nrounds

2.  Final xgb.train at the optimal round on all data
