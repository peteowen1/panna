# Query Local Understat Parquet

Queries local Understat parquet files using DuckDB SQL.

## Usage

``` r
query_local_understat_parquet(
  table_type,
  sql_template,
  league = NULL,
  season = NULL
)
```

## Arguments

- table_type:

  Table type (metadata, roster, shots, events)

- sql_template:

  SQL query template with `{table}` placeholder

- league:

  Optional league filter

- season:

  Optional season filter

## Value

Data frame with query results
