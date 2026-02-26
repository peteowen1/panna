# Build parquet file from RDS files for a league-season

Reads all RDS files for a table_type/league/season and combines them
into a single parquet file. If a parquet file already exists, new RDS
data is merged with existing parquet data (for incremental updates).

## Usage

``` r
build_parquet(table_type, league, season, verbose = TRUE)
```

## Arguments

- table_type:

  Table type (e.g., "summary", "events")

- league:

  League code (e.g., "ENG")

- season:

  Season string (e.g., "2024-2025")

- verbose:

  Print progress messages

## Value

Path to created parquet file, or NULL if no data

## Examples

``` r
if (FALSE) { # \dontrun{
build_parquet("summary", "ENG", "2024-2025")
} # }
```
