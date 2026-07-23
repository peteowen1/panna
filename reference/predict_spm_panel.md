# Score panel rows against a `fit_spm_panel()` model

Rebuilds the design matrix using the SAME global/deviation column spec
the model was fit with (`model$panna_metadata`), so prediction-time
columns are guaranteed identical in name/order to fit-time – required
whenever `newdata` is a different panel slice (e.g. the eval harness
scoring vintage `Y`'s rows against a model trained through `Y`).

## Usage

``` r
predict_spm_panel(model, newdata, lambda = c("min", "1se"))
```

## Arguments

- model:

  A
  [`fit_spm_panel()`](https://peteowen1.github.io/panna/reference/fit_spm_panel.md)
  result.

- newdata:

  Panel-shaped data.table/data.frame (needs `model`'s `predictor_cols`
  and, if the model used role pooling, `role_group`).

- lambda:

  `"min"` (default) or `"1se"`.

## Value

data.table(player_id, vintage_year (if present), pred).

## See also

Other spm panel:
[`assert_asof_panel_window()`](https://peteowen1.github.io/panna/reference/assert_asof_panel_window.md),
[`assert_grouped_player_folds()`](https://peteowen1.github.io/panna/reference/assert_grouped_player_folds.md),
[`build_spm_panel()`](https://peteowen1.github.io/panna/reference/build_spm_panel.md),
[`classify_role_group()`](https://peteowen1.github.io/panna/reference/classify_role_group.md),
[`fit_spm_panel()`](https://peteowen1.github.io/panna/reference/fit_spm_panel.md),
[`fit_spm_panel_xgb()`](https://peteowen1.github.io/panna/reference/fit_spm_panel_xgb.md),
[`make_grouped_player_foldid()`](https://peteowen1.github.io/panna/reference/make_grouped_player_foldid.md),
[`predict_spm_panel_net()`](https://peteowen1.github.io/panna/reference/predict_spm_panel_net.md),
[`predict_spm_panel_xgb()`](https://peteowen1.github.io/panna/reference/predict_spm_panel_xgb.md)
