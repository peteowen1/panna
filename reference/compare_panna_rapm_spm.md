# Compare panna, RAPM, and SPM ratings

Analyzes differences between the three rating types.

## Usage

``` r
compare_panna_rapm_spm(panna_ratings, rapm_ratings, spm_ratings)
```

## Arguments

- panna_ratings:

  Data frame of panna ratings

- rapm_ratings:

  Data frame of RAPM ratings

- spm_ratings:

  Data frame of SPM ratings

## Value

Data frame comparing all rating types

## Examples

``` r
if (FALSE) { # \dontrun{
comparison <- compare_panna_rapm_spm(panna_ratings, rapm_ratings, spm_ratings)
head(comparison)
} # }
```
