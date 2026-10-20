# Each player's usual position before each match (the `asof_` level)

For every row, the position he played most minutes in over matches on
EARLIER dates only: never the same day, never later, so a 2016 row is
not decided by 2024 minutes. The vote uses the `window_days` before the
match (default 365, as `.role8_asof()` does for PSR) and falls back to
his whole earlier career when that window is empty (back from a long
injury). Each grouping is voted on separately, so `asof_role8` is his
most common role8, not a coarsening of his most common `role_full`. Ties
go to the first code in level order.

## Usage

``` r
position_asof(player_match, window_days = 365L)
```

## Arguments

- player_match:

  One row per player-match: `player_id`, `match_date`, `minutes`,
  `match_role_full`, `match_role_source` (e.g.
  [`position_match()`](https://peteowen1.github.io/panna/reference/position_match.md)
  output with date and minutes joined on).

- window_days:

  Trailing window in days; `Inf` for the whole earlier career.

## Value

A data.table in the row order of `player_match` with `asof_role_full`,
`asof_role16`, `asof_role8`, `asof_role4` and `asof_role_source`:
"window" (voted in the trailing window), "all_prior" (window empty,
whole earlier career used) or "none" (no earlier position; every role
column `NA`).

## Details

Only rows whose `match_role_source` is a real position vote ("opta",
"opta_sub", "slot", "slot_table"), weighted by minutes.

## See also

Other positions:
[`position_career()`](https://peteowen1.github.io/panna/reference/position_career.md),
[`position_match()`](https://peteowen1.github.io/panna/reference/position_match.md),
[`position_roles()`](https://peteowen1.github.io/panna/reference/position_roles.md),
[`position_season()`](https://peteowen1.github.io/panna/reference/position_season.md),
[`position_slot_table()`](https://peteowen1.github.io/panna/reference/position_slot_table.md)
