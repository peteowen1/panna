# Calculate panna rating

Core panna rating calculation combining RAPM with SPM prior. Formula:
beta_panna = beta_diff + beta_spm Where beta_diff is from: min \|\|y -
X\*beta\|\|^2 + lambda \* \|\|beta - beta_spm\|\|^2

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

## Examples

``` r
if (FALSE) { # \dontrun{
rapm_data <- prepare_rapm_data(splint_data)
spm_ratings <- calculate_spm_ratings(player_features, spm_model)
panna <- calculate_panna_rating(rapm_data, spm_ratings, lambda_prior = 1)
head(panna$ratings)
} # }
```
