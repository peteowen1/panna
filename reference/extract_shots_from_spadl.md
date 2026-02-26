# Extract Shots from xG-Scored SPADL for Splint Pipeline

Extracts shot rows from SPADL data that has been scored with xG
predictions, and formats them for the splint creation pipeline. This
bridges the xMetrics infrastructure (SPADL + xG model) with the
RAPM/splint pipeline.

## Usage

``` r
extract_shots_from_spadl(spadl, lineups)
```

## Arguments

- spadl:

  SPADL actions data frame with `xg` and `is_penalty` columns (from
  [`add_xg_to_spadl()`](https://peteowen1.github.io/panna/reference/add_xg_to_spadl.md)
  and penalty detection).

- lineups:

  Lineup data from
  [`load_opta_lineups()`](https://peteowen1.github.io/panna/reference/load_opta_lineups.md)
  for team_name mapping.

## Value

Data frame with one row per shot containing:

- match_id, minute, team, player_id, player_name

- xg: Model-predicted xG (penalties overridden to 0.76)

- is_goal: Whether the shot resulted in a goal

- is_penalty: Whether the shot was a penalty
