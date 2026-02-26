# Validate panna ratings

Calculates validation metrics for panna ratings.

## Usage

``` r
validate_panna_ratings(panna_ratings, rapm_ratings, spm_ratings)
```

## Arguments

- panna_ratings:

  Panna ratings

- rapm_ratings:

  RAPM ratings

- spm_ratings:

  SPM ratings

## Value

List of validation metrics

## Examples

``` r
if (FALSE) { # \dontrun{
validation <- validate_panna_ratings(panna_ratings, rapm_ratings, spm_ratings)
validation$panna_rapm_cor
validation$panna_spm_cor
} # }
```
