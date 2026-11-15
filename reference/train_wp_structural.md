# Train the structural (home-frame) win probability model

Fits \\H\\ = P(home team wins; draw = 0.5) from home score margin, time
remaining, red-card difference, half, extra time and the pre-match Elo
gap, optionally plus who has the ball. See
[`predict_wp`](https://peteowen1.github.io/panna/reference/predict_wp.md)
for how \\H\\ becomes the possession team's win chance, and panna#273
for why. Nrounds is chosen by 5-fold match-grouped CV with early
stopping, as in
[`train_wp_model`](https://peteowen1.github.io/panna/reference/train_wp_model.md).

## Usage

``` r
train_wp_structural(
  wp_features,
  use_possession = TRUE,
  nrounds = 600L,
  max_depth = 4L,
  eta = 0.1,
  nfolds = 5L,
  min_child_weight = 50L,
  early_stopping_rounds = 20L,
  seed = 42L,
  weight = NULL,
  extra_features = character()
)

predict_wp_structural(wp_model, wp_features, team_agnostic = FALSE)
```

## Arguments

- wp_features:

  Output of
  [`create_wp_features`](https://peteowen1.github.io/panna/reference/create_wp_features.md)
  with `wp_label`, joined to Elo by
  [`add_wp_match_elo`](https://peteowen1.github.io/panna/reference/add_wp_match_elo.md).

- use_possession:

  Add `home_has_ball` as an input (default `TRUE`: the SEP model Pete
  chose 2026-11-10). With strength in the model this measures what
  possession is worth beyond team quality; 2026-11-10 EPL hold-out: 1.6
  points, no gain in log loss.

- nrounds, max_depth, eta, nfolds, min_child_weight,
  early_stopping_rounds, seed:

  XGBoost / CV settings.

- weight:

  Optional per-row training weights (same length as the rows kept after
  dropping NA labels), e.g. to undo row subsampling.

- extra_features:

  Names of further numeric columns in `wp_features` to use as inputs, in
  the HOME frame or frame-free (e.g. minutes into stoppage time). No
  monotone constraint; NA allowed. Stored on the model, so prediction
  needs the same columns.

- wp_model:

  A model from `train_wp_structural()`.

- team_agnostic:

  Score with every team-strength input set to 0 – the Elo gap and, when
  the model has it, the starting-XI gap `xi_diff` – i.e. as if the two
  teams were equally strong (the site's team-agnostic WPA view). Home
  advantage stays in.

## Value

A list of class `panna_wp_structural` with `model`, `feature_names`,
`use_possession`, `cv_logloss`, `optimal_nrounds` and
`type = "structural"`.

`predict_wp_structural()`: the possession team's win chance per row,
live threat included via the next-goal mixture.

## See also

Other win probability:
[`add_wp_match_elo()`](https://peteowen1.github.io/panna/reference/add_wp_match_elo.md),
[`add_wp_vars()`](https://peteowen1.github.io/panna/reference/add_wp_vars.md),
[`add_wp_xi_strength()`](https://peteowen1.github.io/panna/reference/add_wp_xi_strength.md),
[`aggregate_player_game_wpa()`](https://peteowen1.github.io/panna/reference/aggregate_player_game_wpa.md),
[`assign_wpa_credit()`](https://peteowen1.github.io/panna/reference/assign_wpa_credit.md),
[`create_wp_features()`](https://peteowen1.github.io/panna/reference/create_wp_features.md),
[`load_wp_model()`](https://peteowen1.github.io/panna/reference/load_wp_model.md),
[`predict_wp()`](https://peteowen1.github.io/panna/reference/predict_wp.md),
[`save_wp_model()`](https://peteowen1.github.io/panna/reference/save_wp_model.md),
[`train_wp_model()`](https://peteowen1.github.io/panna/reference/train_wp_model.md),
[`wpa_build_ledger()`](https://peteowen1.github.io/panna/reference/wpa_build_ledger.md)
