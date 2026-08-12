# Score single-match box-score stat lines with spm_value prices

Given match-level per-90 statistics and role classifications, scores
each row using the C1 context-adjusted SPM prices.

## Usage

``` r
calculate_spm_value(match_stats, coefs = NULL, gd_scale = 1)
```

## Arguments

- match_stats:

  data.frame/data.table with per-90 box-score features and position
  roles.

- coefs:

  List output of
  [`load_spm_value_coefficients()`](https://peteowen1.github.io/panna/reference/load_spm_value_coefficients.md).

- gd_scale:

  Numeric multiplier to anchor to GD scale units (default 1.0, derived
  by GD scale calibration).

## Value

data.table with `spm_value_off`, `spm_value_def`, and net `spm_value`.

## See also

Other spm_value:
[`calculate_value_context_gap()`](https://peteowen1.github.io/panna/reference/calculate_value_context_gap.md),
[`load_spm_value_coefficients()`](https://peteowen1.github.io/panna/reference/load_spm_value_coefficients.md)
