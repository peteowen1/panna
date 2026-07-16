# Save EPV Model

Saves trained EPV model to disk.

## Usage

``` r
save_epv_model(epv_model, path = NULL)
```

## Arguments

- epv_model:

  EPV model from fit_epv_model()

- path:

  Directory to save model. If NULL, uses pannadata/data/opta/models/

## Value

Invisibly returns path

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
[`predict_xg()`](https://peteowen1.github.io/panna/reference/predict_xg.md),
[`predict_xgot()`](https://peteowen1.github.io/panna/reference/predict_xgot.md),
[`predict_xpass()`](https://peteowen1.github.io/panna/reference/predict_xpass.md)
