# Validate SQL column names

Checks that column names contain only safe characters to prevent SQL
injection.

## Usage

``` r
validate_sql_columns(columns)
```

## Arguments

- columns:

  Character vector of column names

## Value

The validated column names (unchanged if valid)
