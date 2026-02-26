# Fit xPass Model using XGBoost

Trains an XGBoost classifier to predict pass completion probability.

## Usage

``` r
fit_xpass_model(
  pass_features,
  nfolds = 5,
  max_depth = 6,
  eta = 0.1,
  subsample = 0.8,
  colsample_bytree = 0.8,
  nrounds = 500,
  early_stopping_rounds = 50,
  verbose = 1
)
```

## Arguments

- pass_features:

  Data frame from prepare_passes_for_xpass()

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

List with fitted model and metadata

## Examples

``` r
if (FALSE) { # \dontrun{
passes <- prepare_passes_for_xpass(spadl)
xpass_model <- fit_xpass_model(passes)
} # }
```
