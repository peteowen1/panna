# Add Opponent's Next Chain Outcome

For EPV conceding model, we need to know if the opponent scores on their
next possession after we lose the ball. Optimized with data.table.

## Usage

``` r
add_next_chain_outcome(chain_outcomes)
```

## Arguments

- chain_outcomes:

  Data frame from classify_chain_outcomes()

## Value

Chain outcomes with next_chain_goal column added
