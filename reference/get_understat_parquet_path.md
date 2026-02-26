# Get Understat parquet file path

Returns path using hierarchical structure:
`{pannadata_dir}/understat/{table_type}/{league}/{season}.parquet`

## Usage

``` r
get_understat_parquet_path(table_type, league, season, create = TRUE)
```

## Arguments

- table_type:

  Table type (metadata, roster, shots, events)

- league:

  League code

- season:

  Season string

- create:

  Whether to create parent directory if missing (default TRUE)

## Value

Path to parquet file
