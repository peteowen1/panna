# Aggregate net goals to one row per player-match

Turns the payment ledger into the frame the rating layer consumes. The
column contract deliberately matches
[`aggregate_player_game_epv()`](https://peteowen1.github.io/panna/reference/aggregate_player_game_epv.md)'s
– `player_id`, `player_name`, `match_date`, `minutes_played`,
`epv_offensive`, `epv_defensive` – so
[`calculate_epr_regression()`](https://peteowen1.github.io/panna/reference/calculate_epr_regression.md)
can be pointed at either without changing, and the two can be gated
against each other on identical footing.

## Usage

``` r
ng_player_game(pay, lineups, verbose = TRUE)
```

## Arguments

- pay:

  Payments from
  [`ng_spread_pools()`](https://peteowen1.github.io/panna/reference/ng_spread_pools.md).

- lineups:

  Opta lineups, for minutes, names, date, league and season.

- verbose:

  Print a summary. Default `TRUE`.

## Value

One row per player-match: identifiers, `minutes_played`, `net_goals`,
`epv_offensive`, `epv_defensive`, and one `ng_*` column per role so a
rating layer can choose its own grouping rather than inheriting this one
(torp D3: store tags, group later).

## Details

**The offence/defence split means something different here, and it is
the better of the two.**
[`aggregate_player_game_epv()`](https://peteowen1.github.io/panna/reference/aggregate_player_game_epv.md)
splits by bucketing action types (passing and shooting are offensive,
tackles and keeper handling defensive), which is presentational –
re-bucketing an action changes the split and not the total. Net goals
splits by which half of the double entry the payment sits on: `offence`
is the side that acted, `defence` the side that conceded. Every player
is paid on both halves of every action his team is involved in, so a
defender who never touches the ball still has a defensive number, which
is the whole point.

Pools must be spread first. A payment with no player is real value, and
dropping it here would quietly shrink a player-game total while every
team total stayed correct – so an unspread pool aborts rather than being
skipped.

## See also

Other net_goals:
[`ng_adjust_for_rating()`](https://peteowen1.github.io/panna/reference/ng_adjust_for_rating.md),
[`ng_build_adjacency()`](https://peteowen1.github.io/panna/reference/ng_build_adjacency.md),
[`ng_build_ledger()`](https://peteowen1.github.io/panna/reference/ng_build_ledger.md),
[`ng_check_conservation()`](https://peteowen1.github.io/panna/reference/ng_check_conservation.md),
[`ng_check_team_totals()`](https://peteowen1.github.io/panna/reference/ng_check_team_totals.md),
[`ng_fold_unpublished()`](https://peteowen1.github.io/panna/reference/ng_fold_unpublished.md),
[`ng_reconcile_margin()`](https://peteowen1.github.io/panna/reference/ng_reconcile_margin.md),
[`ng_shares()`](https://peteowen1.github.io/panna/reference/ng_shares.md),
[`ng_spread_pools()`](https://peteowen1.github.io/panna/reference/ng_spread_pools.md)
