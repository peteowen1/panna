# Calculate panna rating (simple single-season API)

Simplified panna rating calculation for single-season use cases.
Combines RAPM with SPM prior using a fixed lambda via
[`glmnet::glmnet()`](https://glmnet.stanford.edu/reference/glmnet.html).

## Usage

``` r
calculate_panna_rating(rapm_data, spm_ratings, lambda_prior = 1, alpha = 0)
```

## Arguments

- rapm_data:

  RAPM data from prepare_rapm_data

- spm_ratings:

  SPM ratings from calculate_spm_ratings

- lambda_prior:

  Regularization strength toward SPM prior

- alpha:

  Elastic net mixing (default 0 for ridge)

## Value

List with panna ratings and model details

## Details

Formula: beta_panna = beta_diff + beta_spm Where beta_diff is from: min
\|\|y - X\*beta\|\|^2 + lambda \* \|\|beta - beta_spm\|\|^2

## Production use

The multi-league pipeline uses
[`fit_rapm_with_prior()`](https://peteowen1.github.io/panna/reference/fit_rapm_with_prior.md)
instead, which:

- Selects lambda via cross-validation (`cv.glmnet`)

- Supports separate offense/defense priors

- Handles league-season covariates

Use this function for quick single-season analyses or prototyping.

## See also

[`fit_rapm_with_prior()`](https://peteowen1.github.io/panna/reference/fit_rapm_with_prior.md)
for the cross-validated production variant.

## Examples

``` r
if (FALSE) { # \dontrun{
rapm_data <- prepare_rapm_data(splint_data)
spm_ratings <- calculate_spm_ratings(player_features, spm_model)
panna <- calculate_panna_rating(rapm_data, spm_ratings, lambda_prior = 1)
head(panna$ratings)
} # }
```
