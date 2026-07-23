# Export SPM coefficients to a CSV for live per-match scoring (panna#173)

Writes a `stat_name, beta, sd` CSV in the same shape as
`inst/extdata/blend_{psr,osr,dsr}_coefficients.csv`, so
`scripts/build-stat-value-coefficients.mjs` (inthegame-blog) can pick it
up with the same pattern already used for PSR/OSR/DSR.

## Usage

``` r
export_spm_coefficients_csv(model, out_path, lambda = "min")
```

## Arguments

- model:

  Fitted SPM glmnet model (from
  [`fit_spm_model`](https://peteowen1.github.io/panna/reference/fit_spm_model.md)
  /
  [`fit_spm_opta`](https://peteowen1.github.io/panna/reference/fit_spm_opta.md)),
  e.g. `spm_glmnet`, `offense_spm_glmnet`, or `defense_spm_glmnet` from
  a saved `05_spm.rds`.

- out_path:

  File path to write the CSV to.

- lambda:

  Which lambda to use ("min" or "1se"), passed through to
  [`extract_spm_coefficients`](https://peteowen1.github.io/panna/reference/extract_spm_coefficients.md).

## Value

Invisibly, the data frame that was written.

## Details

Unlike PSR/OSR/DSR, whose training features are TEAM-SUMS (see
[`calculate_psr`](https://peteowen1.github.io/panna/reference/calculate_psr.md)'s
`coef_df` docs, panna#167) and therefore need an exported `sd` to
standardize a served per-player raw value,
[`fit_spm_model()`](https://peteowen1.github.io/panna/reference/fit_spm_model.md)
trains directly on individual PLAYER-level per-90 features via
`glmnet::cv.glmnet(x = X, ..., standardize = TRUE)` – glmnet un-does its
own internal standardization before
[`coef()`](https://rdrr.io/r/stats/coef.html) returns, so
[`extract_spm_coefficients`](https://peteowen1.github.io/panna/reference/extract_spm_coefficients.md)'s
output is already on the raw per-player feature scale. `sd` is written
as 1 for every row (a harmless no-op divisor) purely for schema parity
with the other coefficient files, not because scoring needs it. Verified
2026-07-23: hand-scoring `raw_value * beta` (summed + intercept) against
this export reproduces
[`calculate_spm_ratings`](https://peteowen1.github.io/panna/reference/calculate_spm_ratings.md)'s
own predictions to floating-point precision (~1e-16) on the full
production player table.
