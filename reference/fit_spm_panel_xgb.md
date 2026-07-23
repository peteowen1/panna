# Fit the XGBoost half of the S6 panel SPM (player-grouped CV)

The panel repeats players across vintages, so
[`fit_spm_xgb()`](https://peteowen1.github.io/panna/reference/fit_spm_xgb.md)'s
random `xgb.cv` folds would leak players between train/test and overfit
the round count (the plan's R5 hazard). This fit builds its folds with
[`make_grouped_player_foldid()`](https://peteowen1.github.io/panna/reference/make_grouped_player_foldid.md)
(every player in exactly one fold, asserted) and mirrors
[`fit_spm_panel()`](https://peteowen1.github.io/panna/reference/fit_spm_panel.md)'s
weighting/complete-case handling. Promoted to the package after three
script copies (09c/13c/05_spm integration, 2026-07-22 Wave 4).

## Usage

``` r
fit_spm_panel_xgb(
  panel,
  target = c("offense", "defense"),
  predictor_cols = NULL,
  weight_transform = c("linear", "sqrt"),
  nfolds = 5,
  seed = 1,
  max_depth = 4,
  eta = 0.1,
  subsample = 0.8,
  colsample_bytree = 0.8,
  nrounds = 500,
  early_stopping_rounds = 20
)
```

## Arguments

- panel:

  Output of
  [`build_spm_panel()`](https://peteowen1.github.io/panna/reference/build_spm_panel.md)
  (or a `vintage_year`-subset).

- target:

  `"offense"` or `"defense"` (fits `<target>_target`).

- predictor_cols:

  Feature columns (default: canonical
  `.spm_opta_predictor_cols(panel)`).

- weight_transform:

  `"linear"` (S4a/S6 parity, default) or `"sqrt"`.

- nfolds, seed:

  Grouped-CV config.

- max_depth, eta, subsample, colsample_bytree, nrounds,
  early_stopping_rounds:

  XGBoost knobs (defaults = the Wave-2-validated panel config, identical
  to the 09c/13c bake-off scripts; NB production 05_spm.R's legacy
  [`fit_spm_xgb()`](https://peteowen1.github.io/panna/reference/fit_spm_xgb.md)
  calls use eta=0.02/nrounds=1000 — different model, different tuning).

## Value

An `xgb.Booster` with `panna_metadata` (type "spm_panel_xgb", target,
predictor_cols, best_nrounds, cv_rmse).

## See also

Other spm panel:
[`assert_asof_panel_window()`](https://peteowen1.github.io/panna/reference/assert_asof_panel_window.md),
[`assert_grouped_player_folds()`](https://peteowen1.github.io/panna/reference/assert_grouped_player_folds.md),
[`build_spm_panel()`](https://peteowen1.github.io/panna/reference/build_spm_panel.md),
[`classify_role_group()`](https://peteowen1.github.io/panna/reference/classify_role_group.md),
[`fit_spm_panel()`](https://peteowen1.github.io/panna/reference/fit_spm_panel.md),
[`make_grouped_player_foldid()`](https://peteowen1.github.io/panna/reference/make_grouped_player_foldid.md),
[`predict_spm_panel()`](https://peteowen1.github.io/panna/reference/predict_spm_panel.md),
[`predict_spm_panel_net()`](https://peteowen1.github.io/panna/reference/predict_spm_panel_net.md),
[`predict_spm_panel_xgb()`](https://peteowen1.github.io/panna/reference/predict_spm_panel_xgb.md)
