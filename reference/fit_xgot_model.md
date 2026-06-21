# Fit xGOT model using XGBoost

Mirrors fit_xg_model() (same XGBoost binary:logistic setup) but trains
on on-target shots with placement features added. Calibrate target: mean
predicted xGOT should approximate the on-target goal rate.

## Usage

``` r
fit_xgot_model(
  shot_features,
  exclude_penalties = TRUE,
  nfolds = 5,
  max_depth = 6,
  eta = 0.05,
  subsample = 0.8,
  colsample_bytree = 0.8,
  nrounds = 500,
  early_stopping_rounds = 50,
  verbose = 1
)
```

## Arguments

- shot_features:

  Data frame from prepare_shots_for_xgot().

- exclude_penalties:

  Exclude penalties from training (default TRUE).

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

  Maximum boosting rounds (default 500)

- early_stopping_rounds:

  Early stopping patience (default 50)

- verbose:

  Print progress (0=silent, 1=progress)

## Value

List with model, cv_result, importance, calibration, panna_metadata.
