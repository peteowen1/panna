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

  SPADL actions with `wpa`, `player_id`, and optionally
  `receiver_player_id` columns.

- actor_share:

  Fraction of WPA credited to the actor (0-1). Default `WPA_ACTOR_SHARE`
  (0.5). Receiver gets `1 - actor_share`.

## Value

The input data.table with added columns:

- wpa_actor:

  WPA credited to the acting player

- wpa_receiver:

  WPA credited to the receiver (0 if no receiver)
