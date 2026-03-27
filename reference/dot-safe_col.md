# Safely extract a column from a data frame, returning zeros if missing

Returns the column as numeric with NAs replaced by 0. If the column
doesn't exist, returns a vector of zeros. Used by SPM feature
calculations.

## Usage

``` r
.safe_col(df, col_name)
```

## Arguments

- df:

  Data frame

- col_name:

  Column name to extract

## Value

Numeric vector (same length as nrow(df)), NAs replaced with 0
