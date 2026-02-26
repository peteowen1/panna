# Internal function to load table data

Common implementation for all load\_\* functions. For remote loading,
first tries individual parquet files (fast). If those don't exist (404),
falls back to ZIP-based loading.

## Usage

``` r
load_table_data(table_type, league, season, source)
```

## Arguments

- table_type:

  The type of table to load (e.g., "summary", "events")

- league:

  Optional league filter

- season:

  Optional season filter

- source:

  "remote" or "local"

## Value

Data frame
