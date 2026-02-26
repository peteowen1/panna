# Query Remote Parquet with SQL (Primary Method)

Downloads a parquet file from GitHub releases to a temp file, then runs
a SQL query on it using DuckDB. This is much faster than downloading
entire ZIP files because:

1.  Download is done once with optimized HTTP

2.  DuckDB queries local file (no network latency per query)

3.  Only filtered/aggregated results come into R memory

## Usage

``` r
query_remote_parquet(
  table_name,
  sql_template,
  release = NULL,
  tag = "fbref-latest"
)
```

## Arguments

- table_name:

  Character. Name of the parquet file (without .parquet).

- sql_template:

  Character. SQL query template with `{table}` placeholder.

- release:

  Optional. Release info from
  [`get_latest_release()`](https://peteowen1.github.io/panna/reference/get_latest_release.md).

- tag:

  Character. GitHub release tag to download from. Defaults to
  "fbref-latest".

## Value

Data frame with query results.

## Examples

``` r
if (FALSE) { # \dontrun{
# Load all summary data
sql <- "SELECT * FROM {table}"
result <- query_remote_parquet("summary", sql)

# Load filtered data (filtering happens in SQL, not R!)
sql <- "SELECT * FROM {table} WHERE league = 'ENG' AND season = '2023-2024'"
eng_2023 <- query_remote_parquet("summary", sql)
} # }
```
