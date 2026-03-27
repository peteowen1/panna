# Calculate centrality

Power iteration method for centrality on an adjacency matrix.

## Usage

``` r
calculate_centrality_scores(adj, damping = 0.85, max_iter = 100L, tol = 1e-06)
```

## Arguments

- adj:

  Sparse adjacency matrix

- damping:

  Damping factor (0-1)

- max_iter:

  Maximum iterations

- tol:

  Convergence tolerance

## Value

Named numeric vector of centrality scores
