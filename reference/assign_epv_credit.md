# Assign EPV Credit with Turnover Handling

Assigns EPV credit/blame to players. For turnovers (possession changes),
splits the value between the player who lost the ball and who gained it.
For merged duels (Aerials, Take On vs Tackle), assigns zero-sum credit
where winner gain = loser loss.

## Usage

``` r
assign_epv_credit(spadl_with_epv, xpass_model = NULL)
```

## Arguments

- spadl_with_epv:

  SPADL actions with EPV values from calculate_action_epv()

- xpass_model:

  Optional xPass model for pass difficulty weighting

## Value

SPADL actions with credit columns added:

- player_credit: EPV credit to acting player

- receiver_credit: EPV credit to receiver (for turnovers, this is
  positive)

- opponent_credit: EPV credit to duel loser (negative, via
  opponent_player_id)

- xpass: Pass completion probability (for passes)

## See also

Other epv:
[`aggregate_player_xmetrics()`](https://peteowen1.github.io/panna/reference/aggregate_player_xmetrics.md),
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
