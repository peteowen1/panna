# Prepare RAPM data for model fitting

Creates design matrix with covariates for ridge regression. This is the
primary RAPM data preparation function.

## Usage

``` r
prepare_rapm_data(
  splint_data,
  min_minutes = 90,
  target_type = c("xg", "goals", "epv", "wpa"),
  include_covariates = TRUE,
  include_league = NULL,
  include_season = NULL,
  mode = c("od", "net")
)
```

## Arguments

- splint_data:

  Combined splint data from create_all_splints

- min_minutes:

  Minimum minutes for player inclusion

- target_type:

  Type of target variable: "xg" for non-penalty xG (default), "goals"
  for actual goals scored, "epv" for EPV, "wpa" for WPA. Use "goals"
  when shots data unavailable. PSV was removed from RAPM
  (FABLE-PRIOR-FIX-PLAN.md D3).

- include_covariates:

  Whether to include game state covariates

- include_league:

  Whether to include league dummies (for multi-league)

- include_season:

  Whether to include season dummies

- mode:

  Design matrix mode, passed through to
  [`create_rapm_design_matrix`](https://peteowen1.github.io/panna/reference/create_rapm_design_matrix.md).
  `"od"` (default) is the production layout (byte-identical to before
  this parameter existed); `"net"` builds the single-column-per-player
  net design for zero-sum targets like WPA (FABLE-PRIOR-FIX-PLAN.md D2).

## Value

List with all model inputs
