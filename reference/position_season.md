# Each player's position over each season (the `season_` level)

The position he played most minutes in across the season, voted as
[`position_career()`](https://peteowen1.github.io/panna/reference/position_career.md)
does, falling back to his career position for a season with no real
position (a season of unused-sub appearances). A season is the season
END year
([`extract_season_end_year()`](https://peteowen1.github.io/panna/reference/extract_season_end_year.md)),
so a calendar-year league and a European season ending the same year are
one season. Uses the whole season by design: season tables and pages
only, never a model input.

## Usage

``` r
position_season(player_match)
```

## Arguments

- player_match:

  One row per player-match: `player_id`, `season`, `minutes`,
  `match_role_full`, `match_role_source`.

## Value

A data.table in the row order of `player_match` with `season_role_full`,
`season_role16`, `season_role8`, `season_role4` and `season_role_source`
("season", "career" or "none").

## See also

Other positions:
[`position_asof()`](https://peteowen1.github.io/panna/reference/position_asof.md),
[`position_career()`](https://peteowen1.github.io/panna/reference/position_career.md),
[`position_match()`](https://peteowen1.github.io/panna/reference/position_match.md),
[`position_roles()`](https://peteowen1.github.io/panna/reference/position_roles.md),
[`position_slot_table()`](https://peteowen1.github.io/panna/reference/position_slot_table.md)
