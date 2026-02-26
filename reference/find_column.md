# Find first matching column from candidates

Returns the first column name that exists in the data frame.

## Usage

``` r
find_column(data, candidates)
```

## Arguments

- data:

  Data frame to search

- candidates:

  Character vector of column names to try (in priority order)

## Value

First matching column name, or NULL if none found
