# Load Pre-trained xPass Model

Loads xPass model from saved RDS file.

## Usage

``` r
load_xpass_model(path = NULL)
```

## Arguments

- path:

  Path to model RDS file. If NULL, uses default location.

## Value

Fitted xPass model

## Examples

``` r
if (FALSE) { # \dontrun{
# Load from default pannadata location
xpass_model <- load_xpass_model()

# Load from a specific path
xpass_model <- load_xpass_model("path/to/xpass_model.rds")
} # }
```
