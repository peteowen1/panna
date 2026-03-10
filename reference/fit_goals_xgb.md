# Fit XGBoost Poisson Model for Goal Prediction

Wrapper around
[`.fit_xgb_model`](https://peteowen1.github.io/panna/reference/dot-fit_xgb_model.md)
with Poisson regression defaults.

## Usage

``` r
fit_goals_xgb(
  X,
  y,
  nfolds = 5L,
  params = NULL,
  nrounds = 500L,
  early_stopping = 30L,
  verbose = 1L
)
```

## Arguments

- X:

  Feature matrix

- y:

  Target vector (goal counts)

- nfolds:

  Number of CV folds (default 5)

- params:

  XGBoost parameters (default: Poisson regression with eta=0.05)

- nrounds:

  Max boosting rounds (default 500)

- early_stopping:

  Patience for early stopping (default 30)

- verbose:

  Print progress (default 1)

## Value

List with model, cv_result, best_nrounds, metadata
