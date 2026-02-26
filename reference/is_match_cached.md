# Check if match is cached

Checks if a match has been fully scraped. First checks RDS files, then
falls back to checking parquet files (for CI/CD environments where only
parquet is available).

## Usage

``` r
is_match_cached(league, season, fbref_id, table_types = "metadata")
```

## Arguments

- league:

  League code

- season:

  Season string

- fbref_id:

  FBref match ID

- table_types:

  Character vector of table types (unused, kept for compatibility)

## Value

Logical - TRUE if match has been fully scraped
