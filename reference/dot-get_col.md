# Extract column from data frame with zero fallback

Returns the column as numeric if it exists, otherwise a vector of zeros.
Used internally by player stat aggregation functions.

## Usage

``` r
.get_col(df, col)
```

## Arguments

- df:

  Data frame

- col:

  Column name

## Value

Numeric vector
