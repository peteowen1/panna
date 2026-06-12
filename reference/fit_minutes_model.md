# Fit the two-stage minutes model

Fit the two-stage minutes model

## Usage

``` r
fit_minutes_model(
  train,
  feature_cols,
  train_idx,
  nrounds_clf = 600L,
  nrounds_reg = 800L,
  early_stopping = 30L,
  verbose = TRUE
)
```

## Arguments

- train:

  Data.frame/data.table with features + `minutes_played` target.

- feature_cols:

  Character vector of feature column names.

- train_idx:

  Logical vector identifying training rows (the rest are held out).

- nrounds_clf:

  Max XGBoost rounds for the P(plays) classifier (stage 1). Default 600.

- nrounds_reg:

  Max XGBoost rounds for the `E[mins | plays]` regressor (stage 2).
  Default 800.

- early_stopping:

  Rounds without val improvement before stopping. Default 30.

- verbose:

  Logical.

## Value

List with `play_clf`, `mins_reg`, `feature_cols`, `eval`.
