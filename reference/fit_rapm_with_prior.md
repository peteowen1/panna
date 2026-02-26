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
  penalize_covariates = FALSE
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

## Value

Fitted model with prior adjustment metadata

## Details

For the O/D design matrix:

- offense_prior: SPM-predicted offensive contribution

- defense_prior: SPM-predicted defensive contribution
