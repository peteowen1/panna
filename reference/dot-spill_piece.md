# Write one table to a temp file for .bind_spilled(), keeping only its shape in memory

Skills step 01 used to hold ~340 league-season tables in memory until
the bind; at 2.49M rows x 421 columns the process peaked at 13.6-14.6 GB
on a 16 GB runner (panna#475). Spilling each piece as it is built keeps
the loop's memory flat.

## Usage

``` r
.spill_piece(dt, dir)
```

## Arguments

- dt:

  A data.frame / data.table.

- dir:

  Directory for the file (created if missing).

## Value

A list: `path`, `n` (rows), `cols` (names), `types` (typeof per column),
`classes` (first class per column).
