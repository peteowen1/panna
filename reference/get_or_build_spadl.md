# Get or Build SPADL (with disk cache)

Thin wrapper over
[`convert_opta_to_spadl()`](https://peteowen1.github.io/panna/reference/convert_opta_to_spadl.md)
that caches the result on disk, keyed by `league` and `season`. SPADL
conversion is the single most expensive step in downstream EPV pipelines
(~25% of total runtime per league-season) and is fully deterministic
given raw events, so caching yields a large win on repeat runs and
backfills.

## Usage

``` r
get_or_build_spadl(
  events,
  league,
  season,
  cache_dir = SPADL_CACHE_DIR,
  force_rebuild = FALSE
)
```

## Arguments

- events:

  Raw Opta events from
  [`load_opta_match_events()`](https://peteowen1.github.io/panna/reference/load_opta_match_events.md).

- league:

  League code (e.g. "ENG"). Used only to build the cache key.

- season:

  Season string (e.g. "2024-2025"). Used only to build the cache key.

- cache_dir:

  Directory to read/write the cached `.rds`. Defaults to
  `SPADL_CACHE_DIR` so all pipelines share one cache.

- force_rebuild:

  If `TRUE`, rebuild and overwrite the cache.

## Value

Data frame in SPADL format, identical to
[`convert_opta_to_spadl()`](https://peteowen1.github.io/panna/reference/convert_opta_to_spadl.md).

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
[`load_epv_model()`](https://peteowen1.github.io/panna/reference/load_epv_model.md),
[`load_xg_model()`](https://peteowen1.github.io/panna/reference/load_xg_model.md),
[`load_xgot_model()`](https://peteowen1.github.io/panna/reference/load_xgot_model.md),
[`load_xpass_model()`](https://peteowen1.github.io/panna/reference/load_xpass_model.md),
[`pb_download_epv_models()`](https://peteowen1.github.io/panna/reference/pb_download_epv_models.md),
[`predict_xg()`](https://peteowen1.github.io/panna/reference/predict_xg.md),
[`predict_xgot()`](https://peteowen1.github.io/panna/reference/predict_xgot.md),
[`predict_xpass()`](https://peteowen1.github.io/panna/reference/predict_xpass.md),
[`save_epv_model()`](https://peteowen1.github.io/panna/reference/save_epv_model.md)
