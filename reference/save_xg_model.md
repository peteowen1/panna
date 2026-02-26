# Save xG Model

Saves trained xG model to RDS file.

## Usage

``` r
save_xg_model(xg_model, path = NULL)
```

## Arguments

- xg_model:

  Fitted xG model from fit_xg_model()

- path:

  Path to save model. If NULL, saves to pannadata/opta/models/

## Value

Invisibly returns the path
