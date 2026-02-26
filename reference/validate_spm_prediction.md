# Validate SPM prediction accuracy

Assesses how well SPM predicts RAPM. Supports weighted metrics to match
weighted model fitting - we care more about accuracy for high-minute
players whose RAPM estimates are more reliable.

## Usage

``` r
validate_spm_prediction(
  spm_ratings,
  rapm_ratings,
  weight_by_minutes = TRUE,
  weight_transform = "sqrt"
)
```

## Arguments

- spm_ratings:

  Data frame with SPM predictions (must include total_minutes for
  weighting)

- rapm_ratings:

  Data frame with actual RAPM

- weight_by_minutes:

  Whether to weight metrics by minutes (default TRUE)

- weight_transform:

  Transform for weights: "sqrt" (default), "linear", "log"

## Value

List with validation metrics (both weighted and unweighted)
