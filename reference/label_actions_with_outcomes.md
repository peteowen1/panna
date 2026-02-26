# Label Actions with Chain Outcomes

Adds chain outcome labels to each action for EPV model training.
Optimized with data.table merge.

## Usage

``` r
label_actions_with_outcomes(spadl_with_chains, chain_outcomes)
```

## Arguments

- spadl_with_chains:

  SPADL actions with chain assignments

- chain_outcomes:

  Chain-level outcomes from classify_chain_outcomes()

## Value

SPADL actions with outcome labels
