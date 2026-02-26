# Save xPass Model

Saves trained xPass model to RDS file.

## Usage

``` r
save_xpass_model(xpass_model, path = NULL)
```

## Arguments

- xpass_model:

  Fitted xPass model

- path:

  Path to save. If NULL, saves to pannadata/opta/models/

## Value

Invisibly returns the path

## Examples

``` r
if (FALSE) { # \dontrun{
xpass_model <- fit_xpass_model(pass_features)
save_xpass_model(xpass_model)
save_xpass_model(xpass_model, path = "models/my_xpass_model.rds")
} # }
```
