# Get Available Remote Tables

Returns list of parquet files available in the GitHub release. Useful
for discovering what data is available for remote queries.

## Usage

``` r
get_remote_tables()
```

## Value

Character vector of table names (without .parquet extension).

## Examples

``` r
if (FALSE) { # \dontrun{
# See what tables are available remotely
tables <- get_remote_tables()
print(tables)
} # }
```
