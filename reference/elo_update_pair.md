# Update a pair of dynamic Elo ratings from a result (internal helper)

Standard zero-sum Elo update with a World-Football-Elo goal-difference
multiplier. `K = 0` returns the ratings unchanged.

## Usage

``` r
elo_update_pair(e1, e2, g1, g2, K)
```
