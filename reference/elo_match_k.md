# Look Up Base K for a Match

Look Up Base K for a Match

## Usage

``` r
elo_match_k(league, k_table = ELO_MATCH_TYPE_K, default = ELO_DEFAULT_K)
```

## Arguments

- league:

  Character vector of league codes.

- k_table:

  Named numeric vector mapping league -\> base K. Defaults to
  ELO_MATCH_TYPE_K. Pass a different vector to override per-match (e.g.,
  for the optimization grid search).

- default:

  Base K for any league not in `k_table`. Defaults to ELO_DEFAULT_K.

## Value

Numeric vector of base K values, same length as `league`.
