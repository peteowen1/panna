# Most common position per formation slot, learned from labelled starters

Opta records the formation (qualifier 130) and each starter's slot
(qualifier 131) at kick-off. Across matches, a given formation and slot
is almost always the same position, so this table gives a position to
anyone whose own label is missing: a substitute after a formation
change, or a starter whose lineup row is blank. It describes Opta's slot
numbering, not any player, so building it from every match passed in
does not leak one player's future into his past.

## Usage

``` r
position_slot_table(lineups, events)
```

## Arguments

- lineups:

  Lineup rows with `match_id`, `team_id`, `player_id`, `position`,
  `position_side`, `is_starter`.

- events:

  Opta events with `match_id`, `team_id`, `player_id`, `type_id`,
  `period_id`, `minute`, `qualifier_json` (only types 34 and 40 are
  used).

## Value

One row per formation and slot: `formation`, `slot`, `role_full` (the
most common), `n` (labelled starters seen) and `share` (the share of
them with that `role_full`).

## See also

Other positions:
[`position_asof()`](https://peteowen1.github.io/panna/reference/position_asof.md),
[`position_career()`](https://peteowen1.github.io/panna/reference/position_career.md),
[`position_match()`](https://peteowen1.github.io/panna/reference/position_match.md),
[`position_roles()`](https://peteowen1.github.io/panna/reference/position_roles.md),
[`position_season()`](https://peteowen1.github.io/panna/reference/position_season.md)
