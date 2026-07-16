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

  Learning rate (default 0.1)

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

## See also

Other epv:
[`aggregate_player_xmetrics()`](https://peteowen1.github.io/panna/reference/aggregate_player_xmetrics.md),
[`assign_epv_credit()`](https://peteowen1.github.io/panna/reference/assign_epv_credit.md),
[`convert_opta_to_spadl()`](https://peteowen1.github.io/panna/reference/convert_opta_to_spadl.md),
[`enrich_match_stats_with_xmetrics()`](https://peteowen1.github.io/panna/reference/enrich_match_stats_with_xmetrics.md),
[`fit_xg_model()`](https://peteowen1.github.io/panna/reference/fit_xg_model.md),
[`fit_xgot_model()`](https://peteowen1.github.io/panna/reference/fit_xgot_model.md),
[`fit_xpass_model()`](https://peteowen1.github.io/panna/reference/fit_xpass_model.md),
[`get_or_build_spadl()`](https://peteowen1.github.io/panna/reference/get_or_build_spadl.md),
[`load_epv_model()`](https://peteowen1.github.io/panna/reference/load_epv_model.md),
[`load_xg_model()`](https://peteowen1.github.io/panna/reference/load_xg_model.md),
[`load_xgot_model()`](https://peteowen1.github.io/panna/reference/load_xgot_model.md),
[`load_xpass_model()`](https://peteowen1.github.io/panna/reference/load_xpass_model.md),
[`pb_download_epv_models()`](https://peteowen1.github.io/panna/reference/pb_download_epv_models.md),
[`predict_xg()`](https://peteowen1.github.io/panna/reference/predict_xg.md),
[`predict_xgot()`](https://peteowen1.github.io/panna/reference/predict_xgot.md),
[`predict_xpass()`](https://peteowen1.github.io/panna/reference/predict_xpass.md),
[`save_epv_model()`](https://peteowen1.github.io/panna/reference/save_epv_model.md)
