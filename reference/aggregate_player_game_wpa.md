# Aggregate player WPA per game

Produces one row per player per match with total WPA, actor WPA, and
receiver WPA. Optionally computes per-90 rates and position-centered
values.

## Usage

``` r
aggregate_player_game_wpa(
  spadl_with_wpa,
  lineups = NULL,
  position_center = FALSE
)
```

## Arguments

- spadl_with_wpa:

  SPADL actions with WPA credit columns from
  [`assign_wpa_credit`](https://peteowen1.github.io/panna/reference/assign_wpa_credit.md).

- lineups:

  Optional lineup data with `player_id`, `match_id`, `minutes_played`,
  and optionally `position`.

- position_center:

  Logical. Subtract position-group mean per season to produce `wpa_adj`.
  Default `FALSE`.

## Value

A data.table with one row per player per match:

- player_id, player_name, team_id, match_id:

  Identifiers

- wpa_total:

  Total WPA = actor + receiver

- wpa_as_actor:

  WPA from own actions

- wpa_as_receiver:

  WPA from receiving

- n_wpa_actions:

  Number of actions with non-zero WPA

- max_wpa:

  Largest single-action WPA (peak moment)

- wpa_p90, wpa_as_actor_p90, wpa_as_receiver_p90:

  Per-90 rates

- wpa_adj:

  Position-centered WPA (if position_center = TRUE)

## See also

Other win probability:
[`add_wp_vars()`](https://peteowen1.github.io/panna/reference/add_wp_vars.md),
[`assign_wpa_credit()`](https://peteowen1.github.io/panna/reference/assign_wpa_credit.md),
[`create_wp_features()`](https://peteowen1.github.io/panna/reference/create_wp_features.md),
[`load_wp_model()`](https://peteowen1.github.io/panna/reference/load_wp_model.md),
[`predict_wp()`](https://peteowen1.github.io/panna/reference/predict_wp.md),
[`save_wp_model()`](https://peteowen1.github.io/panna/reference/save_wp_model.md),
[`train_wp_model()`](https://peteowen1.github.io/panna/reference/train_wp_model.md)
