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
