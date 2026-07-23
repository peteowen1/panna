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
  lambda_seq = NULL,
  mode = c("od", "net")
)
```

## Arguments

- rapm_data:

  List from prepare_rapm_data

- offense_prior:

  Named vector of offensive SPM predictions (by player_id). In
  `mode = "net"` this is the single net SPM prior.

- defense_prior:

  Named vector of defensive SPM predictions (by player_id). Must be
  `NULL` when `mode = "net"`.

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

- mode:

  Design matrix mode matching the `rapm_data` the caller built. `"od"`
  (default) expects `_off`/`_def` player columns. `"net"` expects the
  single-column-per-player (`_net`) design from
  `create_rapm_design_matrix(mode = "net")` and requires
  `defense_prior = NULL` (FABLE-PRIOR-FIX-PLAN.md D2/D4).

## Value

Fitted model with prior adjustment metadata

## Details

For the O/D design matrix (`mode = "od"`, default):

- offense_prior: SPM-predicted offensive contribution

- defense_prior: SPM-predicted defensive contribution

For the net design matrix (`mode = "net"`, FABLE-PRIOR-FIX-PLAN.md D2/D4
– e.g. WPA, whose off/def split is mechanically unidentified because the
target is zero-sum): a single per-player column exists, so
`offense_prior` alone carries the net SPM prior and `defense_prior` has
no meaning and must be `NULL`.

## See also

Other rapm:
[`assert_prior_free_target()`](https://peteowen1.github.io/panna/reference/assert_prior_free_target.md),
[`create_all_splints()`](https://peteowen1.github.io/panna/reference/create_all_splints.md),
[`create_rapm_design_matrix()`](https://peteowen1.github.io/panna/reference/create_rapm_design_matrix.md),
[`extract_period_end_times()`](https://peteowen1.github.io/panna/reference/extract_period_end_times.md),
[`extract_player_timing_from_events()`](https://peteowen1.github.io/panna/reference/extract_player_timing_from_events.md),
[`extract_rapm_ratings()`](https://peteowen1.github.io/panna/reference/extract_rapm_ratings.md),
[`fit_rapm()`](https://peteowen1.github.io/panna/reference/fit_rapm.md)
