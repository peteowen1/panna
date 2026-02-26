# Fit SPM model using Opta features

Fits an elastic net model predicting RAPM from Opta box score
statistics. Uses Opta-specific per-90 features for prediction.

## Usage

``` r
fit_spm_opta(
  data,
  alpha = 0.5,
  nfolds = 10,
  weight_by_minutes = TRUE,
  weight_transform = "sqrt"
)
```

## Arguments

- data:

  Data frame from aggregate_opta_stats joined with RAPM ratings

- alpha:

  Elastic net mixing (0=ridge, 1=lasso, default 0.5)

- nfolds:

  Number of CV folds (default 10)

- weight_by_minutes:

  Whether to weight by minutes (default TRUE)

- weight_transform:

  Transform for weighting: "sqrt", "linear", "log"

## Value

Fitted glmnet model with metadata

## Examples

``` r
if (FALSE) { # \dontrun{
# Aggregate Opta stats
opta_features <- aggregate_opta_stats(opta_stats)

# Join with RAPM
spm_data <- opta_features |>
  inner_join(rapm_ratings |> select(player_id, rapm), by = "player_id")

# Fit Opta SPM
opta_spm <- fit_spm_opta(spm_data)
} # }
```
