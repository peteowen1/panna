# Fit SPM model using XGBoost

Fits an XGBoost model predicting RAPM from box score statistics. Uses
xgb.cv to find optimal number of boosting rounds via early stopping.

## Usage

``` r
fit_spm_xgb(
  data,
  predictor_cols = NULL,
  nfolds = 10,
  max_depth = 4,
  eta = 0.1,
  subsample = 0.8,
  colsample_bytree = 0.8,
  nrounds = 500,
  early_stopping_rounds = 20,
  weight_by_minutes = TRUE,
  weight_transform = "sqrt",
  verbose = 1
)
```

## Arguments

- data:

  Data frame from prepare_spm_regression_data or aggregate_player_stats
  joined with RAPM ratings

- predictor_cols:

  Character vector of predictor column names

- nfolds:

  Number of CV folds (default 10)

- max_depth:

  Maximum tree depth (default 4)

- eta:

  Learning rate (default 0.1)

- subsample:

  Row subsampling ratio (default 0.8)

- colsample_bytree:

  Column subsampling ratio (default 0.8)

- nrounds:

  Maximum boosting rounds (default 500, uses early stopping)

- early_stopping_rounds:

  Stop if no improvement for this many rounds (default 20)

- weight_by_minutes:

  Whether to weight observations by total_minutes (default TRUE)

- weight_transform:

  How to transform minutes for weighting: "sqrt", "linear", "log"

- verbose:

  Print progress (0=silent, 1=performance, 2=details)

## Value

List with xgb model, cv results, and metadata
