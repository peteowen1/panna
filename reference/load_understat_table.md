# Load Understat data from parquet cache

Load Understat data from parquet cache

## Usage

``` r
load_understat_table(league, season, understat_id = NULL, table_type)
```

## Arguments

- league:

  League code

- season:

  Season string

- understat_id:

  Optional specific match ID to load

- table_type:

  Type of table

## Value

Data frame or NULL if not cached
