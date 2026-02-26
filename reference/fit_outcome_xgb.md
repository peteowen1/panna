# Fit XGBoost Multinomial Model for Match Outcome

Fits XGBoost multi:softprob for P(Home Win), P(Draw), P(Away Win).
Labels: 0 = Home Win, 1 = Draw, 2 = Away Win.

## Usage

``` r
fit_outcome_xgb(
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

  Integer labels (0=H, 1=D, 2=A)

- nfolds:

  Number of CV folds (default 5)

- params:

  XGBoost parameters (default multinomial)

- nrounds:

  Max boosting rounds (default 500)

- early_stopping:

  Patience for early stopping (default 30)

- verbose:

  Print progress (default 1)

## Value

List with model, cv_result, best_nrounds, metadata
