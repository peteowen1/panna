# Predict xG for Shots

Applies trained xG model to predict goal probability for shots.

## Usage

``` r
predict_xg(xg_model, shot_features)
```

## Arguments

- xg_model:

  Fitted xG model from fit_xg_model()

- shot_features:

  Data frame with shot features (same format as training)

## Value

Vector of xG predictions (probabilities)

## See also

Other epv:
[`aggregate_player_xmetrics()`](https://peteowen1.github.io/panna/reference/aggregate_player_xmetrics.md),
[`assign_epv_credit()`](https://peteowen1.github.io/panna/reference/assign_epv_credit.md),
[`convert_opta_to_spadl()`](https://peteowen1.github.io/panna/reference/convert_opta_to_spadl.md),
[`enrich_match_stats_with_xmetrics()`](https://peteowen1.github.io/panna/reference/enrich_match_stats_with_xmetrics.md),
[`fit_epv_model()`](https://peteowen1.github.io/panna/reference/fit_epv_model.md),
[`fit_xg_model()`](https://peteowen1.github.io/panna/reference/fit_xg_model.md),
[`fit_xgot_model()`](https://peteowen1.github.io/panna/reference/fit_xgot_model.md),
[`fit_xpass_model()`](https://peteowen1.github.io/panna/reference/fit_xpass_model.md),
[`get_or_build_spadl()`](https://peteowen1.github.io/panna/reference/get_or_build_spadl.md),
[`load_epv_model()`](https://peteowen1.github.io/panna/reference/load_epv_model.md),
[`load_xg_model()`](https://peteowen1.github.io/panna/reference/load_xg_model.md),
[`load_xgot_model()`](https://peteowen1.github.io/panna/reference/load_xgot_model.md),
[`load_xpass_model()`](https://peteowen1.github.io/panna/reference/load_xpass_model.md),
[`pb_download_epv_models()`](https://peteowen1.github.io/panna/reference/pb_download_epv_models.md),
[`predict_xgot()`](https://peteowen1.github.io/panna/reference/predict_xgot.md),
[`predict_xpass()`](https://peteowen1.github.io/panna/reference/predict_xpass.md),
[`save_epv_model()`](https://peteowen1.github.io/panna/reference/save_epv_model.md)

## Examples

``` r
if (FALSE) { # \dontrun{
xg_model <- fit_xg_model(training_shots)
new_shots <- prepare_shots_for_xg(new_shot_events)
xg_pred <- predict_xg(xg_model, new_shots)
} # }
```
