# Value each player's named defensive work in a match

The default weight behind `ng_spread_pools(dacts_share = )`. Where
[`.ng_defensive_acts()`](https://peteowen1.github.io/panna/reference/dot-ng_defensive_acts.md)
counts acts and lets a routine clearance weigh the same as a goal-line
block, this uses the ledger's own valuation of the defensive work the
feed *did* name him for, and spreads the work it could not name in
proportion to it.

## Usage

``` r
.ng_defensive_value(pay)
```

## Arguments

- pay:

  Payments from `ng_build_ledger(convention = "team")`.

## Value

`match_id`, `player_id`, `dacts`.

## Details

Only positive payments count. A named defensive payment is a credit
almost by construction – the defender is paid `-v` on an action that
cost the attacker `v` – but the sign is clamped rather than assumed, so
a negative one reduces a player to no claim on the pool instead of a
negative claim, which would invert his share.

This is self-referential by design: the pool is routed by the ledger's
own numbers. `dacts_share` is what keeps it from running away, because
every setting below 1 blends it with a flat share, so a player who was
never named still holds a floor.
