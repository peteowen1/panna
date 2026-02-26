# Save Understat match table to parquet cache

Appends match data to existing parquet file or creates new one. Matches
are identified by understat_id to avoid duplicates. Uses temp file +
rename to avoid Windows file locking issues.

## Usage

``` r
save_understat_table(data, league, season, table_type)
```

## Arguments

- data:

  Data frame to save (single match)

- league:

  League code

- season:

  Season string

- table_type:

  Type of table (metadata, roster, shots)

## Value

Invisible path to saved file
