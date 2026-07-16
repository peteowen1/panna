# Assign WPA credit to players

Splits WPA between the acting player and receiver (for passes). When
there is no receiver (shots, clearances, etc.), the actor gets 100% of
WPA.

## Usage

``` r
assign_wpa_credit(spadl_with_wpa, actor_share = WPA_ACTOR_SHARE)
```

## Arguments

- spadl_with_wpa:

  SPADL actions with `wpa`, `player_id`, `team_id`, and optionally
  `receiver_player_id` + `receiver_team_id` columns.

- actor_share:

  Fraction of WPA credited to the actor (0-1). Default `WPA_ACTOR_SHARE`
  (0.5). Receiver gets `1 - actor_share`.

## Value

The input data.table with added columns:

- wpa_actor:

  WPA credited to the acting player

- wpa_receiver:

  WPA credited to the receiver (0 if no receiver), sign-flipped on
  cross-team transitions

## Details

Cross-team note: WPA is sign-flipped per actor's team in
[`add_wp_vars`](https://peteowen1.github.io/panna/reference/add_wp_vars.md)
so positive wpa always means "good for my team". When the next-action's
player is on the OPPOSITE team (turnover, save, interception, opp shot),
passing the receiver `(1 - actor_share) * wpa` unchanged would credit
the receiver in the actor's team perspective – i.e. a goalkeeper saving
a shot would inherit the shooter's negative WPA. We sign-flip on
cross-team transitions so the receiver is credited in their own team's
perspective (mirrors AFL pattern in worker/src/ep-model.js
scoreChainRows lines 289-333 and afl/match.qmd:600).

## See also

Other win probability:
[`add_wp_vars()`](https://peteowen1.github.io/panna/reference/add_wp_vars.md),
[`aggregate_player_game_wpa()`](https://peteowen1.github.io/panna/reference/aggregate_player_game_wpa.md),
[`create_wp_features()`](https://peteowen1.github.io/panna/reference/create_wp_features.md),
[`load_wp_model()`](https://peteowen1.github.io/panna/reference/load_wp_model.md),
[`predict_wp()`](https://peteowen1.github.io/panna/reference/predict_wp.md),
[`save_wp_model()`](https://peteowen1.github.io/panna/reference/save_wp_model.md),
[`train_wp_model()`](https://peteowen1.github.io/panna/reference/train_wp_model.md)
