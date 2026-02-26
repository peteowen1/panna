# Combine home and away team tables

Merges home and away data for a table type into single data frame.

## Usage

``` r
combine_team_tables(parsed_data, table_type)
```

## Arguments

- parsed_data:

  List containing parsed tables

- table_type:

  Type of table (e.g., "summary", "passing")

## Value

Combined data frame with both teams, or NULL if no data
