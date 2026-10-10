# Route a payment whose recipient the feed does not name into a team pool

A payment with no player would be dropped by any per-player aggregation
– silently, because the team totals are untouched by the loss. Both
conventions need this, and for a while only the margin one had it: the
team branch returned before reaching it, so an action with no attributed
player produced an `actor` row with `player_id = NA` that sat outside
[`ng_spread_pools()`](https://peteowen1.github.io/panna/reference/ng_spread_pools.md)
(which only touches pool roles) and vanished from every player-level
rollup.

## Usage

``` r
.ng_pool_unnamed(pay)
```

## Arguments

- pay:

  Payments with `player_id`, `role`.

## Value

The same payments with unnamed non-pool rows re-tagged as pools.
