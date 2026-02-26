# Fit SPM model

Fits an elastic net model predicting RAPM from box score statistics.
Weights observations by minutes played (sqrt transform) by default to
reduce influence of noisy low-minute players whose RAPM and per-90 stats
are unreliable.

## Usage

``` r
fit_spm_model(
  data,
  predictor_cols = NULL,
  alpha = 0.5,
  nfolds = 10,
  weight_by_minutes = TRUE,
  weight_transform = "sqrt"
)
```

## Arguments

- data:

  Data frame from prepare_spm_regression_data or aggregate_player_stats
  joined with RAPM ratings

- predictor_cols:

  Character vector of predictor column names

- alpha:

  Elastic net mixing (0=ridge, 1=lasso, default 0.5)

- nfolds:

  Number of CV folds

- weight_by_minutes:

  Whether to weight observations by total_minutes (default TRUE).
  Reduces influence of noisy low-minute estimates on model coefficients.

- weight_transform:

  How to transform minutes for weighting: "sqrt" (default) - square root
  of minutes (moderate weighting) "linear" - raw minutes (strong
  weighting toward high-minute players) "log" - log of minutes (gentle
  weighting) "none" - equal weights

## Value

Fitted glmnet model with metadata
