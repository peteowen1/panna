# Validate a parquet file by checking magic bytes

Parquet files must start and end with the 4-byte magic number "PAR1".
Interrupted downloads produce truncated files missing the footer.

## Usage

``` r
validate_parquet_file(path)
```

## Arguments

- path:

  Path to the parquet file.

## Value

TRUE if valid parquet file, FALSE if corrupt, NA if validation could not
be performed (e.g., permission denied).
