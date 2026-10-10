# Build the net goals ledger

Turns per-action `epv_delta` into one payment row per (action,
recipient), in the **home-margin frame**: an away team's action is
negated so a single number conserves. Summed over a match, the payments
equal the goal difference.

## Usage

``` r
ng_build_ledger(
  spadl_with_epv,
  adj = NULL,
  fixtures,
  allocate = TRUE,
  shares = ng_shares(),
  convention = c("team", "margin"),
  lineups = NULL,
  shot_chain = TRUE,
  shot_aftermath = TRUE,
  verbose = TRUE
)
```

## Arguments

- spadl_with_epv:

  Output of
  [`calculate_action_epv()`](https://peteowen1.github.io/panna/reference/calculate_action_epv.md).

- adj:

  Output of
  [`ng_build_adjacency()`](https://peteowen1.github.io/panna/reference/ng_build_adjacency.md).
  Optional: when supplied, the ledger uses the true next actor rather
  than SPADL's post-filter neighbour. 12.01% of actions disagree, so
  leaving it out is a measurably different ledger, not a convenience.

- fixtures:

  Fixtures with `match_id`, `home_team_id`, `away_team_id`. Used only to
  pick the home frame, never to fit anything.

- allocate:

  Apply the credit and blame rules (`TRUE`, the default) or pay every
  row whole to its actor (`FALSE`). The crude mode exists so the
  identity can be asserted with no rule for an error to hide behind.

- shares:

  Output of
  [`ng_shares()`](https://peteowen1.github.io/panna/reference/ng_shares.md).
  Ignored when `allocate = FALSE`.

- convention:

  `"team"` (the default, Pete 2026-09-21) allocates each action twice,
  once per side, so **each team** sums to its own goal difference – +2
  and -2 for a 3-1 win, zero across the match. That is the ESPN Net
  Points convention and what torp ships as of 1.7.0. `"margin"`
  allocates it once, split across both sides, so the **match** sums to
  the goal difference and team totals float. Its pools carry 17.4% of
  absolute ledger value against the team convention's 39.1%, because it
  never books the conceding half – which is most of what nobody is named
  for. Kept for comparison.

- lineups:

  Optional lineups (`match_id`, `team_id`, `player_id`, `position`,
  `sub_off_minute`). Used to name each side's goalkeeper on the finish
  step of a shot that has no save row (a goal). Without it, that step
  goes to the defending side's pool.

- shot_chain:

  If `TRUE` (default), the row after a shot starts from 0, the shot's
  end, instead of the model's restart value, so no value appears between
  them unbooked (309 goals a season on ENG 2024-25). Needs `epv` on the
  actions; ignored without it.

- shot_aftermath:

  If `TRUE` (default), a shot is worth more than its xG:
  `xG + (1 - xG) * A`, where `A` is the expected value of the state
  after a shot that does not score (corners, rebounds, keeping the
  ball), fitted on the season's own non-goal shots. The row before the
  shot is repriced to match, and a non-goal shot ends at the real value
  of the next state rather than 0, so the next toucher no longer
  inherits it. The chain from 0 (`shot_chain`) then applies after goals
  only. Needs `epv` on the actions. May also be a fit from an earlier
  build (`attr(pay, "shot_aftermath_fit")`) to price shots with that
  season's line – for a single match, whose 20-odd shots are too few to
  fit one.

- verbose:

  Print a summary. Default `TRUE`.

## Value

A data.table of payments, one row per (action, recipient): `match_id`,
`action_id`, `player_id`, `team_id`, `is_home`, `role`, `play_type`,
`value_home` (home-margin frame), `value_own` (the player's own frame,
so positive is always good for him).

## Details

The allocation at this stage is deliberately crude – every row pays its
whole `epv_delta` to the acting player. That is obviously wrong about
*who* and provably right about *how much*, which makes it the one clean
moment to assert the identity: there is no rule for an error to hide
behind, and nothing has yet been forced toward a target.
[`ng_reconcile_margin()`](https://peteowen1.github.io/panna/reference/ng_reconcile_margin.md)
does force one, at the very end of the chain, and that is exactly why
this assertion has to happen HERE and stay here. Torp's suite passed 48
assertions against a ledger with the away sign flipped because every
test ran after its reconciler; this function is the assertion point that
avoids repeating that.

Difficulty terms (the decision/surprise split, D-SHOT, defensive
routing) arrive in step 3 and only ever *subdivide* a row's `epv_delta`.
They can never change a match total.

## See also

Other net_goals:
[`ng_adjust_for_rating()`](https://peteowen1.github.io/panna/reference/ng_adjust_for_rating.md),
[`ng_build_adjacency()`](https://peteowen1.github.io/panna/reference/ng_build_adjacency.md),
[`ng_check_conservation()`](https://peteowen1.github.io/panna/reference/ng_check_conservation.md),
[`ng_check_team_totals()`](https://peteowen1.github.io/panna/reference/ng_check_team_totals.md),
[`ng_fold_unpublished()`](https://peteowen1.github.io/panna/reference/ng_fold_unpublished.md),
[`ng_player_game()`](https://peteowen1.github.io/panna/reference/ng_player_game.md),
[`ng_reconcile_margin()`](https://peteowen1.github.io/panna/reference/ng_reconcile_margin.md),
[`ng_shares()`](https://peteowen1.github.io/panna/reference/ng_shares.md),
[`ng_spread_pools()`](https://peteowen1.github.io/panna/reference/ng_spread_pools.md)
