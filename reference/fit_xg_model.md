# Fit xG Model using XGBoost

Trains an XGBoost classifier to predict goal probability from shot
features. Uses cross-validation to find optimal number of boosting
rounds.

## Usage

``` r
fit_xg_model(
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

  Data frame from prepare_shots_for_xg()

- exclude_penalties:

  Whether to exclude penalties from training (default TRUE)

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

List with:

- model: Fitted XGBoost model

- cv_result: Cross-validation results

- importance: Feature importance

- calibration: Calibration metrics

- panna_metadata: Model metadata

## Examples

``` r
if (FALSE) { # \dontrun{
shots <- load_opta_shot_events("ENG", "2024-2025")
shot_features <- prepare_shots_for_xg(shots)
xg_model <- fit_xg_model(shot_features)
} # }
```
