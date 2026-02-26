# Internal function to load Understat table data

Common implementation for all load_understat\_\* functions. Handles both
remote (GitHub releases) and local (parquet/RDS) sources.

## Usage

``` r
load_understat_table_data(table_type, league, season, source)
```

## Arguments

- table_type:

  The type of Understat table (roster, shots, metadata)

- league:

  Optional league filter

- season:

  Optional season filter (single year format)

- source:

  "remote" or "local"

## Value

Data frame
