# Fit RAPM model

Fits ridge regression on the design matrix with:

- Target: xgf90 (xG FOR per 90) or gf90 (goals FOR per 90)

- Player columns: playerX_off, playerX_def

- Covariates: gd, gf, ga, avg_min, is_home

## Usage

``` r
fit_rapm(
  rapm_data,
  alpha = 0,
  nfolds = 10,
  use_weights = TRUE,
  standardize = FALSE,
  penalize_covariates = FALSE,
  parallel = TRUE,
  n_cores = NULL,
  fixed_lambda = NULL
)
```

## Arguments

- rapm_data:

  List from prepare_rapm_data

- alpha:

  Elastic net mixing parameter (0 = ridge, 1 = lasso)

- nfolds:

  Number of CV folds for lambda selection

- use_weights:

  Whether to use splint duration weights

- standardize:

  Whether to standardize predictors

- penalize_covariates:

  Whether to penalize covariate coefficients

- parallel:

  Whether to use parallel processing for CV folds

- n_cores:

  Number of cores (default: half of available)

- fixed_lambda:

  Optional single lambda value. When supplied, skips `cv.glmnet` and
  fits at this lambda directly (see `.glmnet_fixed_lambda`). Default
  `NULL` = cross-validated (current behaviour). Used by the as-of-date
  career-Panna build to avoid re-running CV for every reference date.

## Value

Fitted model with metadata

## Details

The target type is determined by the rapm_data (set in
prepare_rapm_data).
