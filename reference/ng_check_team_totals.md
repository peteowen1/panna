# Check each team's players against that team's own goal difference

The team convention's identity: for a 3-1 win the winners sum to +2 and
the losers to -2, and the match sums to zero. Unlike the margin
convention this is checked per team, not per match.

## Usage

``` r
ng_check_team_totals(pay, fixtures, verbose = TRUE)
```

## Arguments

- pay:

  Payments from `ng_build_ledger(convention = "team")`

- fixtures:

  Fixtures with `match_id`, `home_team_id`, `home_score`, `away_score`

- verbose:

  Print the summary. Default `TRUE`.

## Value

One row per team-match: `own_total`, `own_gd`, `err`.

## Details

Like
[`ng_check_conservation()`](https://peteowen1.github.io/panna/reference/ng_check_conservation.md)
this is a report, not a gate, and it measures the RAW ledger – before
[`ng_reconcile_margin()`](https://peteowen1.github.io/panna/reference/ng_reconcile_margin.md)
forces each team onto its scoreline. That gap is the thing worth
watching: on ENG 2024-2025 it is a median 0.20 goals per team-match, and
the reconciliation that closes it is only safe while it stays small. If
this report moves, the reconciliation is quietly doing more of the work
than the ledger is, which is the failure torp recorded for its
`half_margin` mode (residual 102% of the value, players reordered). Read
this number before trusting the reconciled one.

## See also

Other net_goals:
[`ng_adjust_for_rating()`](https://peteowen1.github.io/panna/reference/ng_adjust_for_rating.md),
[`ng_build_adjacency()`](https://peteowen1.github.io/panna/reference/ng_build_adjacency.md),
[`ng_build_ledger()`](https://peteowen1.github.io/panna/reference/ng_build_ledger.md),
[`ng_check_conservation()`](https://peteowen1.github.io/panna/reference/ng_check_conservation.md),
[`ng_fold_unpublished()`](https://peteowen1.github.io/panna/reference/ng_fold_unpublished.md),
[`ng_player_game()`](https://peteowen1.github.io/panna/reference/ng_player_game.md),
[`ng_reconcile_margin()`](https://peteowen1.github.io/panna/reference/ng_reconcile_margin.md),
[`ng_shares()`](https://peteowen1.github.io/panna/reference/ng_shares.md),
[`ng_spread_pools()`](https://peteowen1.github.io/panna/reference/ng_spread_pools.md)
