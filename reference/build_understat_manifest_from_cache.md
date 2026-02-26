# Build manifest from existing cached Understat data

Scans existing parquet files and builds a manifest. Use this to
bootstrap the manifest when migrating from the old system.

## Usage

``` r
build_understat_manifest_from_cache(data_dir = NULL)
```

## Arguments

- data_dir:

  Base data directory (defaults to pannadata_dir())

## Value

Data frame with manifest structure
