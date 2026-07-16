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
  weight_transform = "sqrt",
  lower_limits = NULL,
  upper_limits = NULL
)
```

## Arguments

- data:

  Data frame from prepare_spm_regression_data or aggregate_opta_stats
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

- lower_limits, upper_limits:

  Optional sign constraints on glmnet coefficients. Accepts a scalar
  (applied to all predictors), an unnamed numeric vector of length
  `ncol(X)`, or a named numeric vector keyed by predictor name
  (unmatched predictors default to `-Inf`/`Inf`). Use to enforce
  directional priors (e.g. negative defensive-tackle coefficient).
  `NULL` (default) = unconstrained.

## Value

Fitted glmnet model with metadata

## See also

Other spm:
[`calculate_spm_ratings()`](https://peteowen1.github.io/panna/reference/calculate_spm_ratings.md),
[`calculate_spm_ratings_xgb()`](https://peteowen1.github.io/panna/reference/calculate_spm_ratings_xgb.md),
[`get_spm_feature_importance()`](https://peteowen1.github.io/panna/reference/get_spm_feature_importance.md)
