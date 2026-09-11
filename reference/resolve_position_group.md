# Resolve each row's position group, ignoring the "Substitute" match role

Opta's `position` is the player's role IN THAT MATCH, so anyone
appearing off the bench is recorded as `"Substitute"` — 394,248 rows in
`01_match_stats.rds`, ~29\\ puts a blend of every position into one
group. This takes each player's modal NON-Substitute position
(minute-weighted, per season, career fallback, then the row's own label
as a last resort) and returns the calibration's groups.

## Usage

``` r
resolve_position_group(dt)
```

## Arguments

- dt:

  data.frame/data.table with `player_id`, `position` and, ideally,
  `season_end_year` and `total_minutes`.

## Value

Character vector of `"GK"`/`"DEF"`/`"MID"`/`"FWD"`/`NA`, one per row of
`dt`.
