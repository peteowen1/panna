# Memoization environment for .get_col warnings

Tracks which missing-column warnings have been emitted by
[`.get_col()`](https://peteowen1.github.io/panna/reference/dot-get_col.md).
Each column name is stored as a key once warned, preventing duplicate
warnings within a session. To reset:
`rm(list = ls(.get_col_warned), envir = .get_col_warned)`

## Usage

``` r
.get_col_warned
```

## Format

An object of class `environment` of length 0.
