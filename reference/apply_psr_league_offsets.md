# Apply cross-league PSR offsets to a PSR table

Adds the per-league offset from
[`compute_psr_league_offsets`](https://peteowen1.github.io/panna/reference/compute_psr_league_offsets.md)
to each row's `psr`, putting weak-league players on a Big-5-equivalent
scale. If `osr`/`dsr` are present, the offset is split evenly so the
`osr + dsr = psr` identity is preserved.

## Usage

``` r
apply_psr_league_offsets(psr_dt, offsets, verbose = FALSE)
```

## Arguments

- psr_dt:

  A data.table/data.frame with a `league` column and a `psr` column
  (optionally `osr`, `dsr`).

- offsets:

  Offset table from `compute_psr_league_offsets` (columns `league`,
  `offset`).

- verbose:

  Report how many rows / leagues were adjusted. Default FALSE.

## Value

`psr_dt` (as data.table) with `psr` (and `osr`, `dsr`) shifted, plus a
`psr_league_offset` column recording the applied value. Rows whose
league has no offset are unchanged (offset 0).

## See also

[`compute_psr_league_offsets`](https://peteowen1.github.io/panna/reference/compute_psr_league_offsets.md)
