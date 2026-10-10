# Split each published player-match's net goals by play type

A published `net_goals` is built in three steps, and the breakdown keeps
each visible so the parts add up exactly:

1.  the ledger's payments to the player
    ([`ng_player_game()`](https://peteowen1.github.io/panna/reference/ng_player_game.md)
    sums them), split here by play type, with the team pool share as its
    own category;

2.  [`ng_fold_unpublished()`](https://peteowen1.github.io/panna/reference/ng_fold_unpublished.md),
    which hands the value of players the published frame does not carry
    (unused or late substitutes) to their team-mates by minutes: "Share
    of unlisted team-mates";

3.  [`ng_reconcile_margin()`](https://peteowen1.github.io/panna/reference/ng_reconcile_margin.md)'s
    anchor to the real goal difference (`ng_recon`).

## Usage

``` r
.ng_breakdown(pay, pre_fold, published, tol = 1e-09)
```

## Arguments

- pay:

  Payment table after
  [`ng_spread_pools()`](https://peteowen1.github.io/panna/reference/ng_spread_pools.md).

- pre_fold:

  [`ng_player_game()`](https://peteowen1.github.io/panna/reference/ng_player_game.md)
  output (before folding), with `net_goals`.

- published:

  The published frame after
  [`ng_reconcile_margin()`](https://peteowen1.github.io/panna/reference/ng_reconcile_margin.md):
  one row per player-match with `net_goals` and `ng_recon`.

- tol:

  Largest allowed gap between a row's categories and its published
  `net_goals`. Rounding level: anything bigger is a dropped or doubled
  payment.

## Value

data.table: `match_id`, `player_id`, `category`, `value` (goals,
positive is good for the player), for every published row with net
goals.
