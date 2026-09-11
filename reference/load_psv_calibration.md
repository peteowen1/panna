# Load the bundled PSV position-calibration table

Reads `inst/extdata/psv_calibration.csv`: per-position factors that put
PSV from every position on a common goals footing. Missing file returns
an empty table and warns, so callers degrade to uncalibrated rather than
error.

## Usage

``` r
load_psv_calibration()
```

## Value

A data.table with `axis`, `level`, `factor` (the shipped,
scale-preserving multiplier), `slope` (duplicate of `factor` for
interface parity with `load_psr_calibration`), `se`, `n_obs`, and
`slope_raw` (the un-normalised fitted slope `factor` was derived from –
see
[`apply_psv_calibration`](https://peteowen1.github.io/panna/reference/apply_psv_calibration.md)
for why the raw slope is not what's applied).

## See also

[`apply_psv_calibration`](https://peteowen1.github.io/panna/reference/apply_psv_calibration.md)
