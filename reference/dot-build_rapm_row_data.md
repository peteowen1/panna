# Build RAPM row data from valid splints

Creates 2 rows per splint (home attacking, away attacking) with game
state covariates and target variable in `"od"` mode (default). In
`"net"` mode, creates 1 row per splint (home-perspective target only) –
see `mode`.

## Usage

``` r
.build_rapm_row_data(valid_splints, target_type, mode = c("od", "net"))
```

## Arguments

- valid_splints:

  Data frame of splints with duration \> 0

- target_type:

  One of "xg", "goals", "epv", "wpa", or "custom". PSV was removed from
  RAPM (FABLE-PRIOR-FIX-PLAN.md D3) – it has its own standalone pipeline
  ([`calculate_psv()`](https://peteowen1.github.io/panna/reference/calculate_psv.md)).

- mode:

  Design matrix mode. `"od"` (default) creates 2 rows per splint (one
  per team-attacking perspective), matched with separate offense/defense
  player columns in
  [`.build_rapm_sparse_matrix`](https://peteowen1.github.io/panna/reference/dot-build_rapm_sparse_matrix.md)
  – the production xg/goals/epv layout, byte-identical to before this
  parameter existed. `"net"` creates 1 row per splint with the
  home-perspective target only, for zero-sum targets like WPA where an
  offense/defense split is mechanically unidentified
  (FABLE-PRIOR-FIX-PLAN.md D2).

## Value

List with row_data data.frame and target_per90_name string
