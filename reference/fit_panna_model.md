# Fit end-to-end panna model

Complete pipeline from splint data to panna ratings.

## Usage

``` r
fit_panna_model(
  splint_data,
  player_features,
  min_minutes = 180,
  lambda_prior = 1,
  validate = TRUE
)
```

## Arguments

- splint_data:

  Combined splint data

- player_features:

  Player feature matrix

- min_minutes:

  Minimum minutes for inclusion

- lambda_prior:

  Regularization strength

- validate:

  Whether to run validation

## Value

List with panna model and all intermediate results

## Examples

``` r
if (FALSE) { # \dontrun{
panna_model <- fit_panna_model(splint_data, player_features, min_minutes = 180)
head(get_panna_ratings(panna_model))
} # }
```
