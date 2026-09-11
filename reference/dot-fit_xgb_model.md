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
  verbose = 1L,
  group_ids = NULL
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

- group_ids:

  Optional vector, one entry per row of `X`/`y` (e.g. `match_id`). When
  supplied, CV folds are assigned by group via
  [`.grouped_cv_folds`](https://peteowen1.github.io/panna/reference/dot-grouped_cv_folds.md)
  instead of xgboost's default random per-row assignment, so a match and
  its mirrored twin never split across folds. `NULL` (default) keeps the
  prior per-row random behaviour.

## Value

List with model, cv_result, best_nrounds, metadata
