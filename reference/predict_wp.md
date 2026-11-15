# Predict win probability

Scores each action's game state with win probability using a trained
model.

## Usage

``` r
predict_wp(wp_model, wp_features, team_agnostic = FALSE)
```

## Arguments

- wp_model:

  Output of
  [`train_wp_model`](https://peteowen1.github.io/panna/reference/train_wp_model.md)
  or
  [`train_wp_structural`](https://peteowen1.github.io/panna/reference/train_wp_structural.md)
  (dispatched to
  [`predict_wp_structural`](https://peteowen1.github.io/panna/reference/train_wp_structural.md)).

- wp_features:

  SPADL features from
  [`create_wp_features`](https://peteowen1.github.io/panna/reference/create_wp_features.md)
  (plus `elo_diff` from
  [`add_wp_match_elo`](https://peteowen1.github.io/panna/reference/add_wp_match_elo.md)
  for a structural model).

- team_agnostic:

  Structural models only: score with the Elo gap set to 0.

## Value

Numeric vector of win probabilities for the team performing each action
(possession POV), same length as `nrow(wp_features)`.

## See also

Other win probability:
[`add_wp_match_elo()`](https://peteowen1.github.io/panna/reference/add_wp_match_elo.md),
[`add_wp_vars()`](https://peteowen1.github.io/panna/reference/add_wp_vars.md),
[`add_wp_xi_strength()`](https://peteowen1.github.io/panna/reference/add_wp_xi_strength.md),
[`aggregate_player_game_wpa()`](https://peteowen1.github.io/panna/reference/aggregate_player_game_wpa.md),
[`assign_wpa_credit()`](https://peteowen1.github.io/panna/reference/assign_wpa_credit.md),
[`create_wp_features()`](https://peteowen1.github.io/panna/reference/create_wp_features.md),
[`load_wp_model()`](https://peteowen1.github.io/panna/reference/load_wp_model.md),
[`save_wp_model()`](https://peteowen1.github.io/panna/reference/save_wp_model.md),
[`train_wp_model()`](https://peteowen1.github.io/panna/reference/train_wp_model.md),
[`train_wp_structural()`](https://peteowen1.github.io/panna/reference/train_wp_structural.md),
[`wpa_build_ledger()`](https://peteowen1.github.io/panna/reference/wpa_build_ledger.md)
