# Fit XGBoost Model with Cross-Validation

Shared helper for training XGBoost models with k-fold cross-validation
and early stopping. Used by
[`fit_goals_xgb`](https://peteowen1.github.io/panna/reference/fit_goals_xgb.md)
(Poisson) and
[`fit_outcome_xgb`](https://peteowen1.github.io/panna/reference/fit_outcome_xgb.md)
(multinomial).

## Usage

``` r
.fit_xgb_model(
  X,
  y,
  params,
  nfolds = 5L,
  nrounds = 500L,
  early_stopping = 30L,
  verbose = 1L
)
```

## Arguments

- X:

  Feature matrix

- y:

  Target vector (goal counts for Poisson, integer labels for
  multinomial)

- params:

  XGBoost parameters list (objective, eval_metric, etc.)

- nfolds:

  Number of CV folds (default 5)

- nrounds:

  Max boosting rounds (default 500)

- early_stopping:

  Patience for early stopping (default 30)

- verbose:

  Print progress (default 1)

## Value

List with model, cv_result, best_nrounds, metadata
