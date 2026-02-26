# Aggregate cached Understat data

Loads and combines all cached parquet files for a table type.

## Usage

``` r
aggregate_understat_data(table_type, league = NULL, season = NULL)
```

## Arguments

- table_type:

  Table type (metadata, roster, shots)

- league:

  Optional league filter

- season:

  Optional season filter

## Value

Combined data frame
