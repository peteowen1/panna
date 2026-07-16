# Fit RAPM with SPM prior (xRAPM)

Fits RAPM model shrinking toward SPM predictions instead of zero. This
helps separate players who always appear together by using box score
statistics as a Bayesian prior.

## Usage

``` r
fit_rapm_with_prior(
  rapm_data,
  offense_prior,
  defense_prior,
  alpha = 0,
  nfolds = 10,
  use_weights = TRUE,
  penalize_covariates = FALSE,
  fixed_lambda = NULL,
  lambda_seq = NULL
)
```

## Arguments

- rapm_data:

  List from prepare_rapm_data

- offense_prior:

  Named vector of offensive SPM predictions (by player_id)

- defense_prior:

  Named vector of defensive SPM predictions (by player_id)

- alpha:

  Elastic net mixing parameter (0 = ridge)

- nfolds:

  Number of CV folds

- use_weights:

  Whether to use splint duration weights

- penalize_covariates:

  Whether to penalize covariate coefficients

- fixed_lambda:

  Optional single lambda value. When supplied, skips `cv.glmnet` and
  fits at this lambda directly (see `.glmnet_fixed_lambda`). Default
  `NULL` = cross-validated (current behaviour). Used by the as-of-date
  career-Panna build.

- lambda_seq:

  Optional explicit lambda sequence for `cv.glmnet` (see
  [`fit_rapm`](https://peteowen1.github.io/panna/reference/fit_rapm.md));
  the panna#87 cloud path passes a short grid bracketing the closed-form
  lambda. Ignored when `fixed_lambda` is supplied.

## Value

Fitted model with prior adjustment metadata

## Details

For the O/D design matrix:

- offense_prior: SPM-predicted offensive contribution

- defense_prior: SPM-predicted defensive contribution

## See also

Other rapm:
[`create_all_splints()`](https://peteowen1.github.io/panna/reference/create_all_splints.md),
[`create_rapm_design_matrix()`](https://peteowen1.github.io/panna/reference/create_rapm_design_matrix.md),
[`extract_period_end_times()`](https://peteowen1.github.io/panna/reference/extract_period_end_times.md),
[`extract_player_timing_from_events()`](https://peteowen1.github.io/panna/reference/extract_player_timing_from_events.md),
[`extract_rapm_ratings()`](https://peteowen1.github.io/panna/reference/extract_rapm_ratings.md),
[`fit_rapm()`](https://peteowen1.github.io/panna/reference/fit_rapm.md)
