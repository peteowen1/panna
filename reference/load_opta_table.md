# Internal function to load Opta table data

A season that comes back empty is retried once with its accents removed
(see `.fold_season_label()`); an accented label the data does store is
still tried first.

## Usage

``` r
load_opta_table(table_type, league, season, columns, source = "local")
```

## Arguments

- table_type:

  Table type: "player_stats", "shots", "shot_events", "events",
  "match_events", "lineups", or "fixtures".

- league:

  League code

- season:

  Optional season filter

- columns:

  Optional columns to select

- source:

  "local" or "remote"

## Value

Data frame
