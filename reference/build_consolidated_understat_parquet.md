# Build Consolidated Understat Parquet Files

Creates a single parquet file per table type containing ALL leagues and
seasons. These consolidated files are uploaded to GitHub releases for
fast remote querying.

## Usage

``` r
build_consolidated_understat_parquet(
  table_types = NULL,
  output_dir = NULL,
  verbose = TRUE
)
```

## Arguments

- table_types:

  Character vector of table types to consolidate. Defaults to
  c("roster", "shots", "metadata").

- output_dir:

  Directory to write consolidated parquet files. Defaults to
  pannadata_dir()/consolidated.

- verbose:

  Print progress messages.

## Value

Data frame with table_type, n_rows, size_mb columns.

## Examples

``` r
if (FALSE) { # \dontrun{
# Build all consolidated Understat parquets
build_consolidated_understat_parquet()
} # }
```
