# Compute diagnostic value context gap (psv - spm_value)

Compute diagnostic value context gap (psv - spm_value)

## Usage

``` r
calculate_value_context_gap(psv, spm_value)
```

## Arguments

- psv:

  Numeric vector of PSV values (result-priced per-game metric).

- spm_value:

  Numeric vector of spm_value values (context-priced per-game metric).

## Value

Numeric vector of `psv - spm_value`.

## See also

Other spm_value:
[`calculate_spm_value()`](https://peteowen1.github.io/panna/reference/calculate_spm_value.md),
[`load_spm_value_coefficients()`](https://peteowen1.github.io/panna/reference/load_spm_value_coefficients.md)
