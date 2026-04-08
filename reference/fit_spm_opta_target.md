# Fit SPM for a custom target variable

Convenience wrapper around
[`fit_spm_opta`](https://peteowen1.github.io/panna/reference/fit_spm_opta.md)
that allows fitting SPM on any target column (not just the default
`rapm`). Useful for multi-target RAPM where each value metric (EPV, WPA,
PSV) has its own RAPM rating that needs an SPM predictor.

## Usage

``` r
fit_spm_opta_target(data, target_col = "rapm", ...)
```

## Arguments

- data:

  Player features data with RAPM ratings. Must contain a column named
  `target_col`.

- target_col:

  Name of the target column (e.g., `"rapm_epv"`, `"rapm_wpa"`,
  `"rapm_psv"`). This column is temporarily renamed to `"rapm"` for
  compatibility with
  [`fit_spm_model()`](https://peteowen1.github.io/panna/reference/fit_spm_model.md).

- ...:

  Additional arguments passed to
  [`fit_spm_opta`](https://peteowen1.github.io/panna/reference/fit_spm_opta.md).

## Value

Fitted SPM model (same as `fit_spm_opta`).
