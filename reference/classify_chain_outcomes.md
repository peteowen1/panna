# Classify Chain Outcomes

Determines the outcome of each possession chain (goal, shot, turnover,
etc.) Optimized with data.table.

## Usage

``` r
classify_chain_outcomes(spadl_with_chains)
```

## Arguments

- spadl_with_chains:

  SPADL actions with chain_id from create_possession_chains()

## Value

Data frame of chain-level statistics
