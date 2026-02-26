# Download and query remote Opta match events (per-league files)

Match events are too large for a single consolidated file, so they are
stored as per-league files (`events_{league}.parquet`) in the release.
This function downloads the per-league file and queries it with DuckDB.

## Usage

``` r
query_remote_opta_match_events(
  opta_league,
  season = NULL,
  columns = NULL,
  repo = "peteowen1/pannadata",
  tag = "opta-latest"
)
```

## Arguments

- opta_league:

  League code in Opta format (EPL, La_Liga, etc.)

- season:

  Optional season filter (e.g., "2021-2022")

- columns:

  Optional columns to select

- repo:

  GitHub repository

- tag:

  Release tag

## Value

Data frame with query results
