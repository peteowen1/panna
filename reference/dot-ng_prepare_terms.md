# Prepare actions for allocation

The work both conventions share: who won the ball, whether it was a
turnover at all, the opposing team, and whether a stop has a shot in
front of it. Factored out so the two conventions cannot drift apart on
the definitions they both depend on.

## Usage

``` r
.ng_prepare_terms(dt)
```

## Arguments

- dt:

  Prepared actions from
  [`ng_build_ledger()`](https://peteowen1.github.io/panna/reference/ng_build_ledger.md)
