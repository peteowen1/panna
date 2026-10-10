# Spread team pools across the players who were on the pitch

A pool payment has a team but no player: it is the pressure, the marking
and the runs nobody is named for. Measured on ENG 2024-2025, the pools
carry **17.4%** of absolute ledger value under `convention = "margin"`
and **39.1%** under `convention = "team"` (which books a conceding half
that is mostly unnamed). Leaving them unspread would read as that share
of football being done by nobody, and would make any comparison between
positions meaningless. Every figure in this file is ENG 2024-2025 unless
it says otherwise, and the convention is named wherever it changes the
number.

## Usage

``` r
ng_spread_pools(
  pay,
  actions,
  lineups,
  dacts_share = 0.5,
  keeper_pool_blame = 1,
  keeper_pool_credit = 1,
  keeper_outside_weight = 0,
  dacts_measure = c("act_value", "count", "named_value"),
  verbose = TRUE
)
```

## Arguments

- pay:

  Payments from
  [`ng_build_ledger()`](https://peteowen1.github.io/panna/reference/ng_build_ledger.md),
  containing `pool_off` / `pool_def` rows.

- actions:

  The action table, for the minute each pool was generated. Needs
  `match_id`, `action_id`, `time_seconds`.

- lineups:

  Opta lineups with `match_id`, `player_id`, `team_id`, `is_starter`,
  `sub_on_minute`, `sub_off_minute`.

- dacts_share:

  How much of the **defensive pool's credit half** to route by each
  player's defensive work rather than flat. 0 is a flat spread; 1 routes
  it entirely by defensive work. Default **0.5** (Pete, 2026-09-21). See
  the note below on why only the credit half, and why the default is 0.

- keeper_pool_blame, keeper_pool_credit:

  A goalkeeper's weight in the **defensive pool's** blame and credit
  halves, relative to an outfield player's 1. Both default 1. Needs
  `position` in `lineups` to find keepers.

- keeper_outside_weight:

  A goalkeeper's weight in any pool for play OUTSIDE his own third.
  Default 0: a keeper shares unnamed value near his own goal, not at the
  other end. Needs `start_x` and `team_id` on `actions`; without them
  every action counts as his own third.

- dacts_measure:

  What "defensive work" means. `"act_value"` (the default) weights each
  act by how much its own `epv_delta` moved the game, so a clearance off
  the line outweighs one on the halfway line. `"count"` counts
  `NG_DEFENSIVE_ACTIONS` instead, treating every act alike.
  `"named_value"` uses the player's named defensive payments –
  **measured and rejected**, see
  [`.ng_defensive_value()`](https://peteowen1.github.io/panna/reference/dot-ng_defensive_value.md).

  `"act_value"` is the default because it is the only one that stays
  monotone without overshooting. Swept on ENG 2024-2025 at `dacts_share`
  0 to 1, the spread across all six positions falls 0.221 -\> 0.061 and
  no position ever becomes an outlier (the keeper lands mid-pack at
  +0.019). `"count"` flips defenders above strikers at 1 and drives the
  keeper to -0.069; `"named_value"` sends him to +0.422. That is a
  behaviour argument, not a spread-minimising one – the spread is not a
  validated target.

- verbose:

  Print a summary. Default `TRUE`.

## Value

The same payments with every pool row replaced by one row per player who
was on the pitch, tagged `role = "pool_off_spread"` /
`"pool_def_spread"`. Pools whose team has no lineup are left unspread
and reported, never silently dropped.

## Details

The spread is **flat across the eleven on the pitch at that minute**.
That is a deliberate starting point, not a result. Torp swept the
alternatives and found that routing by positional mirror alone made the
forward/defender gap *wider*, because a midfielder's mirror is another
midfielder; and that its richer "context" spread (pairings, defensive
acts, mirror, time on ground) narrowed the gap but repeated worse year
over year than a flat spread. Flat is the option that cannot quietly
encode a prior about who deserves it.

Substitutions are honoured to the minute, which matters more in football
than in AFL: a player who came on in the 80th minute is credited only
for pools generated after he came on.

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
[`ng_shares()`](https://peteowen1.github.io/panna/reference/ng_shares.md)
