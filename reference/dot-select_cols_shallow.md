# Select columns without copying them

Builds a table from references to `x`'s existing column vectors, so
narrowing a multi-GB table costs no memory (R copies a vector only when
one side is modified). `x[cols]` on a data.frame is also shallow, but
`dt[, cols, with = FALSE]` deep-copies every column, and step 02 is
already at ~13.7GB when it writes the narrowed copy.

## Usage

``` r
.select_cols_shallow(x, cols)
```

## Arguments

- x:

  A data.frame or data.table.

- cols:

  Column names to keep, all present in `x`.

## Value

An object of `x`'s class with only `cols`.
