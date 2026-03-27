# Build Player Adjacency Matrix

Creates a sparse adjacency matrix where players are connected if they
participated in the same match (as teammates or opponents). Edge weight
= number of shared matches (or sum of minutes if available).

## Usage

``` r
build_player_adjacency(pm)
```

## Arguments

- pm:

  Data frame with player_id, match_id, and optionally minutes

## Value

Sparse symmetric matrix (dgCMatrix)
