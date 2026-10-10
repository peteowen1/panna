# Count each player's defensive acts in a match

The weight behind `ng_spread_pools(dacts_share = )`. Successful tackles,
interceptions, clearances, recoveries, aerials and keeper actions – the
things a player visibly did to win or deny the ball.

## Usage

``` r
.ng_defensive_acts(actions, weight_by_value = FALSE)
```

## Arguments

- actions:

  SPADL actions with `match_id`, `player_id`, `action_type`, `result`,
  and `epv_delta` when `weight_by_value` is `TRUE`.

- weight_by_value:

  Weight each act by how much its own `epv_delta` moved the game rather
  than counting it. Fixes "a routine clearance weighs the same as a
  goal-line block" without reaching for the player's own payments, which
  is what made
  [`.ng_defensive_value()`](https://peteowen1.github.io/panna/reference/dot-ng_defensive_value.md)
  run away.

## Value

`match_id`, `player_id`, `dacts`.

## Details

Two caveats worth stating rather than discovering later. It counts the
**whole match**, so a pool generated in the 10th minute is routed partly
by acts made in the 80th; that is acceptable for a descriptive metric
and would not be for a predictive one. And it counts acts, not their
value, so a routine clearance weighs the same as a goal-line block.
