# Each player's position over his whole career (the `career_` level)

The position he played most minutes in across every match passed in,
voted separately for each grouping, ties to the first code in level
order. Uses later matches by design: for player pages and displays,
never as a model input (use
[`position_asof()`](https://peteowen1.github.io/panna/reference/position_asof.md)
there). Votes as
[`position_asof()`](https://peteowen1.github.io/panna/reference/position_asof.md)
does: only rows whose `match_role_source` is a real position, weighted
by minutes.

## Usage

``` r
position_career(player_match)
```

## Arguments

- player_match:

  One row per player-match: `player_id`, `minutes`, `match_role_full`,
  `match_role_source`.

## Value

A data.table in the row order of `player_match` with `career_role_full`,
`career_role16`, `career_role8`, `career_role4` and `career_role_source`
("career", or "none" with every role `NA`).

## See also

Other positions:
[`position_asof()`](https://peteowen1.github.io/panna/reference/position_asof.md),
[`position_match()`](https://peteowen1.github.io/panna/reference/position_match.md),
[`position_roles()`](https://peteowen1.github.io/panna/reference/position_roles.md),
[`position_season()`](https://peteowen1.github.io/panna/reference/position_season.md),
[`position_slot_table()`](https://peteowen1.github.io/panna/reference/position_slot_table.md)
