# Fit EPV Model

Trains an XGBoost model to predict expected possession value. Supports
two methods:

- "goal": Multinomial classification (who scores next:
  team/opponent/nobody)

- "xg": Regression on signed xG of next shot (+team, -opponent, 0 if
  none)

## Usage

``` r
fit_epv_model(
  features,
  labels,
  method = c("goal", "xg"),
  nfolds = 5,
  max_depth = 6,
  eta = 0.1,
  subsample = 0.8,
  colsample_bytree = 0.8,
  nrounds = 1000,
  early_stopping_rounds = 50,
  verbose = 1
)
```

## Arguments

- features:

  Data frame from create_epv_features()

- labels:

  Data frame with labels (next_goal_label for "goal", next_xg_label for
  "xg")

- method:

  Either "goal" (multinomial) or "xg" (regression). Default "goal".

- nfolds:

  Number of CV folds (default 5)

- max_depth:

  Maximum tree depth (default 6)

- eta:

  Learning rate (default 0.05)

- subsample:

  Row subsampling (default 0.8)

- colsample_bytree:

  Column subsampling (default 0.8)

- nrounds:

  Maximum boosting rounds (default 1000)

- early_stopping_rounds:

  Early stopping patience (default 50)

- verbose:

  Print progress (default 1)

## Value

Fitted EPV model with metadata
