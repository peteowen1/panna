# Create Calibration Table

Groups predictions into bins and compares predicted vs actual
probabilities.

## Usage

``` r
calibration_table(y_true, prob_matrix, n_bins = 10L)
```

## Arguments

- y_true:

  Integer vector of true outcomes (0=H, 1=D, 2=A)

- prob_matrix:

  Matrix with 3 columns of predicted probabilities

- n_bins:

  Number of calibration bins (default 10)

## Value

Data frame with bin midpoints, predicted and actual probabilities
