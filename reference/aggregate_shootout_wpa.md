# Aggregate shootout WPA per player across matches

Runs
[`score_shootout_kicks`](https://peteowen1.github.io/panna/reference/score_shootout_kicks.md)
on every match's shootout kicks, then rolls the result up to one row per
player, combining their TAKER WPA (own kicks) with their KEEPER WPA
(saves they made facing the other team's kicks).

## Usage

``` r
aggregate_shootout_wpa(
  kicks_all,
  lineups = NULL,
  keeper_save_share = 0.5,
  n_regulation = 5L
)
```

## Arguments

- kicks_all:

  A data.frame/data.table of shootout kicks across one or more matches:
  shot-outcome events (`type_id` 16/15/14/13, `period_id >= 5`) with
  `match_id`, `team_id`, `player_id`, `player_name`, `scored`, and
  orderable `minute`/`second` (or pre-sorted within match).

- lineups:

  Optional lineup table with `match_id`, `team_id`, `player_id`,
  `player_name`, `position`, `minutes_played` — used to resolve the
  saving keeper per match.

- keeper_save_share:

  Passed to
  [`score_shootout_kicks`](https://peteowen1.github.io/panna/reference/score_shootout_kicks.md).
  Default 0.5.

- n_regulation:

  Regulation kicks per team. Default 5.

## Value

A data.table, one row per player, with: `player_id`, `player_name`,
`kicks_taken`, `kicks_scored`, `taker_wpa` (sum over own kicks),
`keeper_wpa` (sum over saves made), `shootout_wpa_total`
(`taker_wpa + keeper_wpa`).

## Details

Keeper resolution: a saved kick's `keeper_wpa` belongs to the defending
team but the shot event names only the taker. We resolve the specific
keeper by joining `lineups` — the opposing team's goalkeeper in that
match (the `position == "Goalkeeper"` player who was on the pitch at the
shootout; if a match lists several, the one with the most minutes, i.e.
the end-of-match keeper who actually faced the kicks). If no lineups are
supplied, keeper WPA is still summed at team level but cannot be
attributed to a player and is dropped from the per-player total
(reported separately as `unattributed_keeper_wpa`).

## See also

Other penalty shootouts:
[`is_shootout_period()`](https://peteowen1.github.io/panna/reference/is_shootout_period.md),
[`score_shootout_kicks()`](https://peteowen1.github.io/panna/reference/score_shootout_kicks.md),
[`shootout_win_prob()`](https://peteowen1.github.io/panna/reference/shootout_win_prob.md)
