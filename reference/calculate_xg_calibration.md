# Calculate xG Model Calibration

Assesses how well predicted probabilities match actual goal rates.

## Usage

``` r
calculate_xg_calibration(actual, predicted, n_bins = 10)
```

## Arguments

- actual:

  Binary vector of actual outcomes (0/1)

- predicted:

  Predicted probabilities

- n_bins:

  Number of calibration bins (default 10)

## Value

List with calibration metrics
