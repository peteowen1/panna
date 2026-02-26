# Build Consolidated Parquet Files for Remote Queries

Creates a single parquet file per table type containing ALL leagues and
seasons. These consolidated files are uploaded to GitHub releases for
fast remote querying (like bouncer's approach). Users can then filter by
league/season in SQL.

## Usage

``` r
build_consolidated_parquet(
  table_types = NULL,
  output_dir = NULL,
  verbose = TRUE
)
```

## Arguments

- table_types:

  Character vector of table types to consolidate. Defaults to all FBref
  table types.

- output_dir:

  Directory to write consolidated parquet files. Defaults to
  pannadata_dir()/consolidated.

- verbose:

  Print progress messages.

## Value

Data frame with table_type, n_rows, size_mb columns.

## Details

This function reads all the individual league/season parquet files for
each table type and combines them into a single file. The resulting
files can be uploaded directly to GitHub releases as individual assets,
enabling fast remote queries via
[`query_remote_parquet()`](https://peteowen1.github.io/panna/reference/query_remote_parquet.md).

Output files are named `{table_type}.parquet` (e.g., `summary.parquet`).

## Examples

``` r
if (FALSE) { # \dontrun{
# Build all consolidated parquets
build_consolidated_parquet()

# Build only summary and events
build_consolidated_parquet(table_types = c("summary", "events"))
} # }
```
