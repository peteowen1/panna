# Check the ledger against the scoreline

The identity is `sum(value_home) == home_score - away_score` per match.
It is **approximate** in football, unlike torp's: panna's EPV asks "who
scores next *this half*" and the shot override swaps model EPV for xG,
so the telescoping terms nearly but not quite cancel. Measured on ENG
2024-2025 the median error is 0.198 goals and the worst match 1.339.

## Usage

``` r
ng_check_conservation(pay, fixtures, verbose = TRUE)
```

## Arguments

- pay:

  Output of
  [`ng_build_ledger()`](https://peteowen1.github.io/panna/reference/ng_build_ledger.md)

- fixtures:

  Fixtures with `match_id`, `home_score`, `away_score`

- verbose:

  Print the summary. Default `TRUE`.

## Value

A data.table, one row per match: `ledger`, `gd`, `err`.

## Details

This function is deliberately a *report*, not a gate: it prints what the
gap is rather than forcing it to zero. Anything that forces the total
would make every downstream assertion vacuous.

## See also

Other net_goals:
[`ng_adjust_for_rating()`](https://peteowen1.github.io/panna/reference/ng_adjust_for_rating.md),
[`ng_build_adjacency()`](https://peteowen1.github.io/panna/reference/ng_build_adjacency.md),
[`ng_build_ledger()`](https://peteowen1.github.io/panna/reference/ng_build_ledger.md),
[`ng_check_team_totals()`](https://peteowen1.github.io/panna/reference/ng_check_team_totals.md),
[`ng_fold_unpublished()`](https://peteowen1.github.io/panna/reference/ng_fold_unpublished.md),
[`ng_player_game()`](https://peteowen1.github.io/panna/reference/ng_player_game.md),
[`ng_reconcile_margin()`](https://peteowen1.github.io/panna/reference/ng_reconcile_margin.md),
[`ng_shares()`](https://peteowen1.github.io/panna/reference/ng_shares.md),
[`ng_spread_pools()`](https://peteowen1.github.io/panna/reference/ng_spread_pools.md)
