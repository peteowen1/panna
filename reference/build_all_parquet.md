# Build all parquet files

Iterates through all table_type/league/season combinations and creates
parquet files. Discovers combinations from both existing RDS files and
existing parquet files (for incremental updates on CI).

## Usage

``` r
build_all_parquet(
  table_types = NULL,
  leagues = NULL,
  seasons = NULL,
  verbose = TRUE
)
```

## Arguments

- table_types:

  Character vector of table types to process. Default: all standard
  table types.

- leagues:

  Optional character vector of leagues to process. If NULL, processes
  all leagues found.

- seasons:

  Optional character vector of seasons to process. If NULL, processes
  all seasons found.

- verbose:

  Print progress messages

## Value

Data frame with table_type, league, season, n_matches, size_mb columns

## Examples

``` r
if (FALSE) { # \dontrun{
# Build all parquet files
build_all_parquet()

# Build only summary for England
build_all_parquet(table_types = "summary", leagues = "ENG")
} # }
```
