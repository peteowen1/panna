# Load shipping spm_value coefficients

Reads the shipping `spm_value_coefficients.csv` from `inst/extdata/` (or
package installation directory). Returns a list with `offense` and
`defense` coefficient vectors (keyed by feature/deviation column names,
including `(Intercept)`).

## Usage

``` r
load_spm_value_coefficients(file_path = NULL)
```

## Arguments

- file_path:

  Optional path to custom coefficient CSV. If `NULL` (default), loads
  from `inst/extdata/spm_value_coefficients.csv`.

## Value

List with `offense` and `defense` data.tables or named numeric vectors.

## See also

Other spm_value:
[`calculate_spm_value()`](https://peteowen1.github.io/panna/reference/calculate_spm_value.md),
[`calculate_value_context_gap()`](https://peteowen1.github.io/panna/reference/calculate_value_context_gap.md)
