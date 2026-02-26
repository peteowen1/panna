# Extract SPM coefficients

Gets feature weights from fitted SPM model.

## Usage

``` r
extract_spm_coefficients(model, lambda = "min")
```

## Arguments

- model:

  Fitted SPM model from fit_spm_model

- lambda:

  Which lambda to use ("min" or "1se")

## Value

Named vector of coefficients
