# Get match IDs from parquet file

Reads a parquet file and extracts unique match IDs. Results are cached
in memory for the session to avoid repeated file reads.

## Usage

``` r
get_parquet_fbref_ids(table_type, league, season)
```

## Arguments

- table_type:

  Table type (e.g., "metadata")

- league:

  League code

- season:

  Season string

## Value

Character vector of match IDs, or NULL if no parquet
