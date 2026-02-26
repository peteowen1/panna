# Get stat tiers for optimization

Classifies stats into tiers for optimization. Tier 1 stats get
individual lambda optimization. Tier 2 stats share a group lambda.

## Usage

``` r
get_stat_tiers()
```

## Value

Named list with elements `tier1` (individually optimized), `tier2`
(group optimized), and `efficiency` (attempt-weighted group).
