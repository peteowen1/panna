# Position-centre per-game net goals for the rating layer

The two-layer split, made explicit. The ledger conserves and therefore
carries no baseline: the moment a positional mean is subtracted, a
team's players stop summing to its goal difference. Predicting is a
different job with different rules, so the centring belongs here,
downstream, where breaking conservation is allowed and useful.

## Usage

``` r
ng_adjust_for_rating(player_game, positions, by_season = TRUE, verbose = TRUE)
```

## Arguments

- player_game:

  Output of
  [`ng_player_game()`](https://peteowen1.github.io/panna/reference/ng_player_game.md),
  or any per-game frame with `player_id`, `epv_offensive` and
  `epv_defensive` – the gate runs this on the production credit layer
  too, so both arms are centred identically and only the allocation
  differs.

- positions:

  A player-to-position map with `player_id` and `position`, e.g. from
  [`get_player_positions()`](https://peteowen1.github.io/panna/reference/get_player_positions.md).
  Rows whose position is unknown are centred on the all-player mean
  rather than dropped, and reported.

- by_season:

  Centre within season as well as position. Default `TRUE`.

- verbose:

  Print a summary. Default `TRUE`.

## Value

The same frame with `epv_offensive` and `epv_defensive` replaced by
their centred values, the originals kept as `*_raw`, and `net_goals_raw`
preserved. The column names are deliberately unchanged so this is a
drop-in for
[`calculate_epr_regression()`](https://peteowen1.github.io/panna/reference/calculate_epr_regression.md).

## Details

**What this mirrors, and why it is needed for a fair comparison.**
Production EPR is fed `epv_offensive_adj` / `epv_defensive_adj` renamed
to the raw names
(`data-raw/match-predictions-opta/build_epr_weekly.R:63-66`) – the
position-centred columns produced at export by `10b_export_game_logs.R`,
not the raw ones.
[`ng_player_game()`](https://peteowen1.github.io/panna/reference/ng_player_game.md)
emits raw net goals. Feeding those two to
[`calculate_epr_regression()`](https://peteowen1.github.io/panna/reference/calculate_epr_regression.md)
unchanged would compare a centred input against an uncentred one and
attribute the difference to the ledger, which it is not. This function
removes that confound.

Centring is per position **and season**: position means drift between
seasons, and a single pooled mean would carry one season's shape into
another. Measured on ENG 2024-2025 the means run from +0.088 for a
striker to -0.035 for a defender per player-game – small, real, and
exactly the systematic offset a rating should not reward or punish a
player for.

## See also

Other net_goals:
[`ng_build_adjacency()`](https://peteowen1.github.io/panna/reference/ng_build_adjacency.md),
[`ng_build_ledger()`](https://peteowen1.github.io/panna/reference/ng_build_ledger.md),
[`ng_check_conservation()`](https://peteowen1.github.io/panna/reference/ng_check_conservation.md),
[`ng_check_team_totals()`](https://peteowen1.github.io/panna/reference/ng_check_team_totals.md),
[`ng_fold_unpublished()`](https://peteowen1.github.io/panna/reference/ng_fold_unpublished.md),
[`ng_player_game()`](https://peteowen1.github.io/panna/reference/ng_player_game.md),
[`ng_reconcile_margin()`](https://peteowen1.github.io/panna/reference/ng_reconcile_margin.md),
[`ng_shares()`](https://peteowen1.github.io/panna/reference/ng_shares.md),
[`ng_spread_pools()`](https://peteowen1.github.io/panna/reference/ng_spread_pools.md)
