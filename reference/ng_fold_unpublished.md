# Fold value owed to unpublished players back into their team

The ledger pays every player who was on the pitch, because the pool
spread reaches all eleven. The published game-logs frame is built from
[`aggregate_player_game_epv()`](https://peteowen1.github.io/panna/reference/aggregate_player_game_epv.md),
which is ACTION-driven: a substitute who came on for a minute and never
touched the ball has no actions, so he has no row. Measured on ENG
2024-2025 that is 45 of 11,472 rows – every one of them a substitute
with 1 to 12 minutes, all genuinely in the lineup.

## Usage

``` r
ng_fold_unpublished(ng, published, verbose = TRUE)
```

## Arguments

- ng:

  Per-game ledger output from
  [`ng_player_game()`](https://peteowen1.github.io/panna/reference/ng_player_game.md).
  Needs `match_id`, `player_id`, `team_id`, `minutes_played`,
  `net_goals`, `ng_offensive`, `ng_defensive`.

- published:

  A frame with the `match_id` + `player_id` pairs that will be
  published.

- verbose:

  Print what was folded. Default `TRUE`.

## Value

`ng` restricted to the published pairs, with each team's totals
preserved exactly.

## Details

Joining the ledger onto that frame therefore drops their value, and a
match's two sides stop cancelling: 0.116 goals instead of ~1e-14. **That
is a publishing artefact, not model error, and it is a different problem
from the 0.20-goal median gap to the scoreline that
[`ng_reconcile_margin()`](https://peteowen1.github.io/panna/reference/ng_reconcile_margin.md)
closes.** Nothing here is forced toward a target; the value simply has
to land somewhere real. Running this FIRST is what keeps the
reconciliation honest – otherwise the gap those 45 rows leave would be
charged to the reconciliation as if the model had produced it.

So it goes back to the team, spread across the team-mates the frame does
carry, by minutes. That is the same rule
[`.ng_pool_unnamed()`](https://peteowen1.github.io/panna/reference/dot-ng_pool_unnamed.md)
already applies one layer up: a payment with no publishable recipient
becomes the team's. After this, a published team's rows sum to exactly
what the ledger gave that team, and the only remaining gap to the
scoreline is the model's.

## See also

Other net_goals:
[`ng_adjust_for_rating()`](https://peteowen1.github.io/panna/reference/ng_adjust_for_rating.md),
[`ng_build_adjacency()`](https://peteowen1.github.io/panna/reference/ng_build_adjacency.md),
[`ng_build_ledger()`](https://peteowen1.github.io/panna/reference/ng_build_ledger.md),
[`ng_check_conservation()`](https://peteowen1.github.io/panna/reference/ng_check_conservation.md),
[`ng_check_team_totals()`](https://peteowen1.github.io/panna/reference/ng_check_team_totals.md),
[`ng_player_game()`](https://peteowen1.github.io/panna/reference/ng_player_game.md),
[`ng_reconcile_margin()`](https://peteowen1.github.io/panna/reference/ng_reconcile_margin.md),
[`ng_shares()`](https://peteowen1.github.io/panna/reference/ng_shares.md),
[`ng_spread_pools()`](https://peteowen1.github.io/panna/reference/ng_spread_pools.md)
