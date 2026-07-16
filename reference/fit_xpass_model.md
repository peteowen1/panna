# Fit xPass Model using XGBoost

Trains an XGBoost classifier to predict pass completion probability.

## Usage

``` r
fit_xpass_model(
  pass_features,
  nfolds = 5,
  max_depth = 6,
  eta = 0.1,
  subsample = 0.8,
  colsample_bytree = 0.8,
  nrounds = 500,
  early_stopping_rounds = 50,
  verbose = 1
)
```

## Arguments

- pass_features:

  Data frame from prepare_passes_for_xpass()

- nfolds:

  Number of CV folds (default 5)

- max_depth:

  Maximum tree depth (default 6)

- eta:

  Learning rate (default 0.1)

- subsample:

  Row subsampling (default 0.8)

- colsample_bytree:

  Column subsampling (default 0.8)

- nrounds:

  Maximum boosting rounds (default 500)

- early_stopping_rounds:

  Early stopping patience (default 50)

- verbose:

  Print progress (0=silent, 1=progress)

## Value

List with fitted model and metadata

## See also

Other epv:
[`aggregate_player_xmetrics()`](https://peteowen1.github.io/panna/reference/aggregate_player_xmetrics.md),
[`assign_epv_credit()`](https://peteowen1.github.io/panna/reference/assign_epv_credit.md),
[`convert_opta_to_spadl()`](https://peteowen1.github.io/panna/reference/convert_opta_to_spadl.md),
[`enrich_match_stats_with_xmetrics()`](https://peteowen1.github.io/panna/reference/enrich_match_stats_with_xmetrics.md),
[`fit_epv_model()`](https://peteowen1.github.io/panna/reference/fit_epv_model.md),
[`fit_xg_model()`](https://peteowen1.github.io/panna/reference/fit_xg_model.md),
[`fit_xgot_model()`](https://peteowen1.github.io/panna/reference/fit_xgot_model.md),
[`get_or_build_spadl()`](https://peteowen1.github.io/panna/reference/get_or_build_spadl.md),
[`load_epv_model()`](https://peteowen1.github.io/panna/reference/load_epv_model.md),
[`load_xg_model()`](https://peteowen1.github.io/panna/reference/load_xg_model.md),
[`load_xgot_model()`](https://peteowen1.github.io/panna/reference/load_xgot_model.md),
[`load_xpass_model()`](https://peteowen1.github.io/panna/reference/load_xpass_model.md),
[`pb_download_epv_models()`](https://peteowen1.github.io/panna/reference/pb_download_epv_models.md),
[`predict_xg()`](https://peteowen1.github.io/panna/reference/predict_xg.md),
[`predict_xgot()`](https://peteowen1.github.io/panna/reference/predict_xgot.md),
[`predict_xpass()`](https://peteowen1.github.io/panna/reference/predict_xpass.md),
[`save_epv_model()`](https://peteowen1.github.io/panna/reference/save_epv_model.md)

## Examples

``` r
if (FALSE) { # \dontrun{
passes <- prepare_passes_for_xpass(spadl)
xpass_model <- fit_xpass_model(passes)
} # }
```
