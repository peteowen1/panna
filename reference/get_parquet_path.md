# Get parquet file path for a league-season

Returns the path where parquet files are stored:
`{pannadata_dir}/{table_type}/{league}/{season}.parquet`

## Usage

``` r
get_parquet_path(table_type, league, season, create = FALSE)
```

## Arguments

- table_type:

  Table type (e.g., "summary", "events")

- league:

  League code (e.g., "ENG")

- season:

  Season string (e.g., "2024-2025")

- create:

  If TRUE, create parent directory if missing

## Value

Path to parquet file
