# Ensure column exists with default

Creates a column if it doesn't exist, optionally deriving from another
column.

## Usage

``` r
ensure_column(
  data,
  col_name,
  default = FALSE,
  source_col = NULL,
  pattern = NULL
)
```

## Arguments

- data:

  Data frame

- col_name:

  Name of column to ensure

- default:

  Default value or function to derive value

- source_col:

  Optional source column for pattern matching

- pattern:

  Regex pattern to match in source column

## Value

Data frame with column ensured
