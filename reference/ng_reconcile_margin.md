# Force each team's published rows to sum to that team's own goal difference

The last gap. After
[`ng_fold_unpublished()`](https://peteowen1.github.io/panna/reference/ng_fold_unpublished.md)
a match's two sides cancel to rounding, but neither side lands on the
scoreline: measured on ENG 2024-2025 the median team-match is **0.20
goals** from its own goal difference and the worst is 1.39. The ledger
is antisymmetric by construction and anchored to nothing, so what it
tracks is the EPV the actions generated, not the goals the match
actually produced. Restarts, half-time, and the event types SPADL does
not carry all move the state without booking a payment, and that
difference has to go somewhere or the metric is not net goals.

## Usage

``` r
ng_reconcile_margin(ng, fixtures, verbose = TRUE)
```

## Arguments

- ng:

  Per-game frame from
  [`ng_player_game()`](https://peteowen1.github.io/panna/reference/ng_player_game.md),
  optionally already folded by
  [`ng_fold_unpublished()`](https://peteowen1.github.io/panna/reference/ng_fold_unpublished.md).
  Needs `match_id`, `player_id`, `team_id`, `minutes_played` and
  `net_goals`, plus the two halves under either the `epv_*` or the
  `ng_*` spelling.

- fixtures:

  Fixtures with `match_id`, `home_team_id`, `away_team_id`,
  `home_score`, `away_score`.

- verbose:

  Print the before-and-after error. Default `TRUE`.

## Value

`ng` with `net_goals` and the defensive half adjusted, plus an
`ng_recon` column holding what each player was given, so a reader
tracing one number by hand can see the three parts separately.

## Details

**torp does exactly this and it is why torp reaches 0.000.**
`.np_team_margin()` computes `short = want - tot` per team-match and
spreads it across the side by time on ground. This is the same step in
goals.

### Why this is safe here and the thing torp rejected was not

torp's own docs reject a reconciliation, and the rejection is of a
different step: `.np_reconcile(level = "half_margin")`, measured
*before* the team-sum convention, whose residual was **102% of**
`|net_points|` and reordered players against time on ground. That is a
correction larger than the thing it corrects. Sized the same way before
this was written, panna's is not:

|                                               |        |
|-----------------------------------------------|--------|
| residual as a share of total `|net_goals|`    | 7.6%   |
| correlation, per player-game, before vs after | 0.9972 |
| Spearman on season totals                     | 0.9944 |
| correlation of the residual with minutes      | 0.0008 |

Nobody meaningfully reorders and the residual carries no minutes bias,
so it is not quietly paying whoever was on the pitch longest. It is in
the same range as the `sum`-level reconciliation torp actually ships
(3%).

### Where it lands in the offence/defence split

All of it on the defensive half, which is what torp does (it books the
change against the pool channel, "the honest home for the difference").
The gap is unattributed team-level value, and panna's team pool is
already defence-weighted. Splitting it across both halves pro-rata was
the alternative and it explodes on a player whose two halves nearly
cancel – the same failure torp records from rescaling components.

## See also

Other net_goals:
[`ng_adjust_for_rating()`](https://peteowen1.github.io/panna/reference/ng_adjust_for_rating.md),
[`ng_build_adjacency()`](https://peteowen1.github.io/panna/reference/ng_build_adjacency.md),
[`ng_build_ledger()`](https://peteowen1.github.io/panna/reference/ng_build_ledger.md),
[`ng_check_conservation()`](https://peteowen1.github.io/panna/reference/ng_check_conservation.md),
[`ng_check_team_totals()`](https://peteowen1.github.io/panna/reference/ng_check_team_totals.md),
[`ng_fold_unpublished()`](https://peteowen1.github.io/panna/reference/ng_fold_unpublished.md),
[`ng_player_game()`](https://peteowen1.github.io/panna/reference/ng_player_game.md),
[`ng_shares()`](https://peteowen1.github.io/panna/reference/ng_shares.md),
[`ng_spread_pools()`](https://peteowen1.github.io/panna/reference/ng_spread_pools.md)
