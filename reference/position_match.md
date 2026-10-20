# Each player's position in each match (the `match_` level)

One row per lineup row, with the position he played in that match in
every grouping and where it came from:

## Usage

``` r
position_match(lineups, events, slot_table = NULL)
```

## Arguments

- lineups:

  Lineup rows with `match_id`, `team_id`, `player_id`, `position`,
  `position_side`, `is_starter`, `minutes_played`.

- events:

  Opta events with `match_id`, `team_id`, `player_id`, `type_id`,
  `period_id`, `minute`, `qualifier_json` (only types 34 and 40 are
  used).

- slot_table:

  A
  [`position_slot_table()`](https://peteowen1.github.io/panna/reference/position_slot_table.md)
  result, or `NULL` to build one from `lineups` and `events`. Pass a
  fixed table when scoring a small subset of matches.

## Value

A data.table with `match_id`, `team_id`, `player_id`,
`match_is_starter`, `match_formation`, `match_slot`,
`match_slot_role_full`, `match_role_full`, `match_role16`,
`match_role8`, `match_role4`, `match_role_source`, in the row order of
`lineups`.

## Details

- `opta`: a starter with an Opta position and side.

- `opta_sub`: a substitute, from the position Opta records on his sub-on
  event (qualifiers 292 and 293; on 96-100% of sub-ons from 2015).
  Measured 2026-10-19 on 101,484 subs, this matches where he touched the
  ball as often as his usual position does (39.3% vs 39.6%) and more
  often than his slot (31.8%): the slot is the departing player's.

- `slot`: a substitute with no Opta sub position, or a starter with a
  blank label, who took a slot whose kick-off holder in this match had a
  label, with the formation unchanged since kick-off. He gets that
  holder's position. Opta records the slot on every sub-on event
  (qualifier 145).

- `slot_table`: the formation had changed, or the slot's holder had no
  label, so the position is the most common one for that formation and
  slot
  ([`position_slot_table()`](https://peteowen1.github.io/panna/reference/position_slot_table.md)).

- `none`: he played but no slot is known (no event data for the match).
  Every role column is `NA`; fill from the `asof_` level where a value
  is needed.

- `unused`: an unused substitute. Every role column is `NA`.

In a back three with no full-back slot (3-4-3, 3-5-2, ...), a wide
midfielder (LM / RM) is recorded as a wing-back (LWB / RWB), so role8
groups him with full-backs: that is where he touches the ball.

A player moved by a later formation change keeps the position he started
the match (or came on) in. `match_slot_role_full` keeps the position the
slot implies (for a sub, the departing player's), whatever the source.

## See also

Other positions:
[`position_asof()`](https://peteowen1.github.io/panna/reference/position_asof.md),
[`position_career()`](https://peteowen1.github.io/panna/reference/position_career.md),
[`position_roles()`](https://peteowen1.github.io/panna/reference/position_roles.md),
[`position_season()`](https://peteowen1.github.io/panna/reference/position_season.md),
[`position_slot_table()`](https://peteowen1.github.io/panna/reference/position_slot_table.md)
