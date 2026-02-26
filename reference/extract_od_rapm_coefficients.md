# Extract offensive/defensive RAPM coefficients

Gets separate O-RAPM and D-RAPM from a model fit on O/D matrix.

## Usage

``` r
extract_od_rapm_coefficients(model, lambda = "min")
```

## Arguments

- model:

  Fitted RAPM model with O/D separated matrix

- lambda:

  Which lambda to use

## Value

Data frame with offensive and defensive ratings
