# View a data.table as a plain data.frame without copying its columns

[`as.data.frame()`](https://rdrr.io/r/base/as.data.frame.html) on a
data.table deep-copies every column. On the 422-column skills
match_stats (6.6 GB) that doubled step 08's peak to 13.7 GB of the 16 GB
GHA runner just to call `write_parquet()`. This builds a data.frame
whose columns are the same vectors, so it costs nothing; the parquet
written from it is byte-identical. Do not modify the result in place: it
shares memory with `x`.

## Usage

``` r
.as_df_no_copy(x)
```

## Arguments

- x:

  A data.frame or data.table.

## Value

A plain data.frame sharing `x`'s column vectors.
