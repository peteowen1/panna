# Fit the xDuel models (five contest sub-models)

Fit the xDuel models (five contest sub-models)

## Usage

``` r
fit_duel_model(
  prepped,
  nfolds = 5,
  max_depth = 5,
  eta = 0.1,
  subsample = 0.8,
  colsample_bytree = 0.8,
  nrounds = 500,
  early_stopping_rounds = 30,
  verbose = 1
)
```

## Arguments

- prepped:

  Named list of finalized contest feature tables (from
  `compute_all_duel_preps`, accumulated across training leagues).

- nfolds, max_depth, eta, subsample, colsample_bytree, nrounds,
  early_stopping_rounds, verbose:

  XGBoost controls.

## Value

List of class `duel_model` with the five sub-models + metadata.

## See also

Other xduel:
[`compute_all_duel_preps()`](https://peteowen1.github.io/panna/reference/compute_all_duel_preps.md),
[`compute_duel_woe()`](https://peteowen1.github.io/panna/reference/compute_duel_woe.md),
[`load_duel_model()`](https://peteowen1.github.io/panna/reference/load_duel_model.md),
[`predict_duel()`](https://peteowen1.github.io/panna/reference/predict_duel.md),
[`prepare_duels_from_events()`](https://peteowen1.github.io/panna/reference/prepare_duels_from_events.md),
[`save_duel_model()`](https://peteowen1.github.io/panna/reference/save_duel_model.md)
