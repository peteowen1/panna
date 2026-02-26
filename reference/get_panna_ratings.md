# Get panna ratings from fitted model

Extracts the final panna ratings from a fitted panna model object.
Requires a model fitted with
[`fit_panna_model`](https://peteowen1.github.io/panna/reference/fit_panna_model.md).

## Usage

``` r
get_panna_ratings(panna_model)
```

## Arguments

- panna_model:

  Fitted panna model from
  [`fit_panna_model`](https://peteowen1.github.io/panna/reference/fit_panna_model.md).

## Value

Data frame with player panna ratings.

## Details

To generate ratings, run one of the pipeline scripts:

- Opta (15 leagues):
  `source("data-raw/player-ratings-opta/run_pipeline_opta.R")`

- FBref (Big 5):
  `source("data-raw/player-ratings-fbref/run_pipeline.R")`

## Examples

``` r
if (FALSE) { # \dontrun{
panna_model <- fit_panna_model(splint_data, player_features)
ratings <- get_panna_ratings(panna_model)
head(ratings)
} # }
```
