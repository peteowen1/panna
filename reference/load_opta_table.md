# Internal function to load Opta table data

Internal function to load Opta table data

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
