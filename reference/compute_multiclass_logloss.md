# Compute Multi-Class Log Loss

Compute Multi-Class Log Loss

## Usage

``` r
compute_multiclass_logloss(y_true, prob_matrix, eps = 1e-15)
```

## Arguments

- y_true:

  Integer vector of true labels (0, 1, 2)

- prob_matrix:

  Matrix with 3 columns (P(0), P(1), P(2))

- eps:

  Clipping epsilon to avoid log(0) (default 1e-15)

## Value

Scalar log loss value
