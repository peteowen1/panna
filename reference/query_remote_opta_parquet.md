# Query remote Opta parquet data

Downloads individual consolidated Opta files from GitHub releases and
queries using DuckDB. Each table type is a single consolidated file
(e.g., opta_player_stats.parquet) that is cached for the session.

## Usage

``` r
query_remote_opta_parquet(
  table_type,
  opta_league,
  season = NULL,
  columns = NULL,
  repo = "peteowen1/pannadata",
  tag = "opta-latest"
)
```

## Arguments

- table_type:

  Table type (player_stats, shots, shot_events, events, lineups,
  fixtures)

- opta_league:

  League code in Opta format (EPL, La_Liga, etc.)

- season:

  Optional season filter (e.g., "2021-2022")

- columns:

  Optional columns to select

- repo:

  GitHub repository (default: "peteowen1/pannadata")

- tag:

  Release tag (default: "opta-latest")

## Value

Data frame with query results
