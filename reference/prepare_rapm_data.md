# Prepare RAPM data for model fitting

Creates design matrix with covariates for ridge regression. This is the
primary RAPM data preparation function.

## Usage

``` r
prepare_rapm_data(
  splint_data,
  min_minutes = 90,
  target_type = c("xg", "goals"),
  include_covariates = TRUE,
  include_league = NULL,
  include_season = NULL
)
```

## Arguments

- splint_data:

  Combined splint data from create_all_splints

- min_minutes:

  Minimum minutes for player inclusion

- target_type:

  Type of target variable: "xg" for non-penalty xG (default), "goals"
  for actual goals scored. Use "goals" when shots data unavailable.

- include_covariates:

  Whether to include game state covariates

- include_league:

  Whether to include league dummies (for multi-league)

- include_season:

  Whether to include season dummies

## Value

List with all model inputs
