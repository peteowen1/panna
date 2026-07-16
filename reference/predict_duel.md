# Predict a duel contest win probability

Predict a duel contest win probability

## Usage

``` r
predict_duel(duel_model, features, contest = names(.DUEL_CONTESTS))
```

## Arguments

- duel_model:

  Fitted model from `fit_duel_model`/`load_duel_model`.

- features:

  data.table of features (from `prepare_duels_from_events`).

- contest:

  One of the five contest names.

## Value

Numeric vector of P(win).

## See also

Other xduel:
[`compute_all_duel_preps()`](https://peteowen1.github.io/panna/reference/compute_all_duel_preps.md),
[`compute_duel_woe()`](https://peteowen1.github.io/panna/reference/compute_duel_woe.md),
[`fit_duel_model()`](https://peteowen1.github.io/panna/reference/fit_duel_model.md),
[`load_duel_model()`](https://peteowen1.github.io/panna/reference/load_duel_model.md),
[`prepare_duels_from_events()`](https://peteowen1.github.io/panna/reference/prepare_duels_from_events.md),
[`save_duel_model()`](https://peteowen1.github.io/panna/reference/save_duel_model.md)
