# Query Local Parquet with SQL

Queries local parquet files using DuckDB SQL. This enables efficient
filtering without loading entire datasets into R memory.

## Usage

``` r
query_local_parquet(table_type, sql_template, league = NULL, season = NULL)
```

## Arguments

- table_type:

  Table type (e.g., "summary", "events", "shots")

- sql_template:

  SQL query template with `{table}` placeholder. The placeholder will be
  replaced with the appropriate file pattern.

- league:

  Optional league filter to narrow the parquet files searched.

- season:

  Optional season filter to narrow the parquet files searched.

## Value

Data frame with query results.

## Examples

``` r
if (FALSE) { # \dontrun{
# Query summary data for England
sql <- "SELECT * FROM {table} WHERE league = 'ENG'"
eng_data <- query_local_parquet("summary", sql, league = "ENG")
} # }
```
