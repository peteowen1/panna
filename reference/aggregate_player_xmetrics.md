# Aggregate Player-Level xG/xA/xPass Metrics

Aggregates SPADL-level predictions to player-season totals. Produces
shooting, assisting, and passing metrics per player.

## Usage

``` r
aggregate_player_xmetrics(spadl, lineups, min_minutes = 0, by_match = FALSE)
```

## Arguments

- spadl:

  SPADL actions data frame with xg, xa, xpass columns.

- lineups:

  Lineup data from load_opta_lineups() for minutes played.

- min_minutes:

  Minimum minutes for inclusion (default 0, no filter).

- by_match:

  Logical. If `TRUE`, aggregate to one row per player per *match* (keys
  include `match_id`); if `FALSE` (default), one row per player across
  the supplied SPADL (season-level, the form consumed by player-ratings
  / blog / compare_players). The per-match form feeds the skills
  pipeline's xG join (one row per player-match).

## Value

Data frame with one row per player (or player-match if `by_match`)
containing:

- **Identity**: player_id, player_name, team_name, minutes

- **Shooting**: shots, goals, xg, npxg, goals_minus_xg, xg_per90

- **Assisting**: key_passes, assists, xa, xa_per90

- **Passing**: passes_attempted, passes_completed, sum_xpass,
  xpass_overperformance, xpass_avg

## See also

Other epv:
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
[`predict_xpass()`](https://peteowen1.github.io/panna/reference/predict_xpass.md),
[`save_epv_model()`](https://peteowen1.github.io/panna/reference/save_epv_model.md)
