# Default shares for the net goals allocation

None of these is identifiable from conservation – the identity holds for
every value, which is the design working correctly and also why the goal
difference cannot choose them. They are defaults to start a
year-over-year repeatability search (torp D17), never results.

## Usage

``` r
ng_shares(
  exec_blame = 0.3,
  named_share = 0.7,
  reb_named = 0.4,
  off_pool = 0.1,
  shot_keep = 0.9
)
```

## Arguments

- exec_blame:

  Share of a failed action's value the actor keeps as execution blame.
  The rest is the defence's credit. Torp D7's default.

- named_share:

  Share of the defence's credit going to the player the feed names, with
  the remainder to the defending team pool for the pressure and shape
  nobody is named for.

- reb_named:

  Share of a rebound going to the stopper rather than his team, per Pete
  2026-09-21: the keeper is credited for the stop and charged for
  parrying it back into play.

- off_pool:

  Share of retained attacking value going to the attacking team pool,
  for the runs and structure that created the option. Torp D9.

- shot_keep:

  Share of every step of a shot (strike, finish, aftermath) the shooter
  keeps, the SAME whether the step gains or loses (Pete, 2026-09-23).
  Splitting by sign – 90% of a gain, 30% of a loss – paid shooters
  +612.7 goals a season on ENG 2024-25 against +10 of actual goals minus
  xG, because the xGOT split turns a saved shot into a big gain (the
  strike) and a big loss (the finish). With one share, a shooter's shot
  rows add up to `shot_keep` times his real finishing.

## Value

A named list of shares.

## See also

Other net_goals:
[`ng_adjust_for_rating()`](https://peteowen1.github.io/panna/reference/ng_adjust_for_rating.md),
[`ng_build_adjacency()`](https://peteowen1.github.io/panna/reference/ng_build_adjacency.md),
[`ng_build_ledger()`](https://peteowen1.github.io/panna/reference/ng_build_ledger.md),
[`ng_check_conservation()`](https://peteowen1.github.io/panna/reference/ng_check_conservation.md),
[`ng_check_team_totals()`](https://peteowen1.github.io/panna/reference/ng_check_team_totals.md),
[`ng_fold_unpublished()`](https://peteowen1.github.io/panna/reference/ng_fold_unpublished.md),
[`ng_player_game()`](https://peteowen1.github.io/panna/reference/ng_player_game.md),
[`ng_reconcile_margin()`](https://peteowen1.github.io/panna/reference/ng_reconcile_margin.md),
[`ng_spread_pools()`](https://peteowen1.github.io/panna/reference/ng_spread_pools.md)
