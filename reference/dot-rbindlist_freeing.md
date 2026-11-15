# Bind a list of tables like rbindlist(fill = TRUE), freeing the inputs as it goes

[`data.table::rbindlist()`](https://rdrr.io/pkg/data.table/man/rbindlist.html)
holds every input AND the full result at once, so its peak is twice the
result. Skills step 01 binds ~2.5M player-match rows x 421 columns (8.4
GB a copy), which put the peak at ~16.9 GB and killed 16 GB GitHub
runners (panna#475). This builds the result one column at a time and
drops that column from every input once copied, so the peak is the
result plus one column.

## Usage

``` r
.rbindlist_freeing(tables)
```

## Arguments

- tables:

  A list of data.frames / data.tables (NULL elements skipped). Atomic
  columns only: the filler is a plain `NA`, so a list-column (rbindlist
  fills NULL) or integer64 column would not match rbindlist exactly.
  Opta match stats have neither.

## Value

A data.table, identical to
`rbindlist(tables, use.names = TRUE, fill = TRUE)` for atomic columns.

## Details

Each column is bound by rbindlist itself (one single-column table per
input, a typed-NA column where an input lacks it), so type coercion and
NA filling follow rbindlist's rules. Columns come out in order of first
appearance, as rbindlist(use.names = TRUE, fill = TRUE) gives them.

DESTRUCTIVE: the inputs are emptied column by column. The caller's list
is invalid afterwards; pass it once and drop it.
