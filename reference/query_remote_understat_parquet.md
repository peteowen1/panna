# Query Remote Understat Parquet

Downloads an Understat parquet file from GitHub releases and queries it.

## Usage

``` r
query_remote_understat_parquet(table_name, sql_template)
```

## Arguments

- table_name:

  Full table name including understat\_ prefix

- sql_template:

  SQL query template with `{table}` placeholder

## Value

Data frame with query results
