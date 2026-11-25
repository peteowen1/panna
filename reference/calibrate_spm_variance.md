# Rescale SPM to a target spread for display (panna#168)

SPM predictions are heavily compressed: the elastic net is scored at
`lambda.min` against a modest-spread RAPM target, and the 50/50 blend
with XGBoost flattens it further. This is a purely linear display
transform that stretches (or shrinks) SPM about its weighted mean so its
weighted standard deviation equals `target_sd`. Player order is
preserved exactly, the mean is unchanged, and any skew is kept (it is a
rescale, not a quantile map).

## Usage

``` r
calibrate_spm_variance(spm, target = NULL, target_sd = NULL, weights = NULL)
```

## Arguments

- spm:

  Numeric vector of SPM values. `NA`s are passed through.

- target:

  Optional numeric vector (same length as `spm`) whose weighted sd
  defines the target spread. Ignored when `target_sd` is given.

- target_sd:

  Optional positive number, the sd to scale to.

- weights:

  Optional non-negative weights (e.g. minutes) used for the mean and sd
  of `spm`, and of `target`. Default equal weights.

## Value

Numeric vector the same length as `spm`, with attribute `"scale"`
holding the multiplier applied about the weighted mean.

## Details

It is meant to be written to a *separate* column (e.g. `spm_display`).
Do not feed the result to the xRAPM prior path: the Bayesian shrinkage
there is calibrated against the raw `spm`.

Give either `target_sd` directly or `target` (e.g. the RAPM the model
was trained on), whose weighted sd is used.

## See also

Other spm:
[`calculate_spm_ratings()`](https://peteowen1.github.io/panna/reference/calculate_spm_ratings.md),
[`calculate_spm_ratings_xgb()`](https://peteowen1.github.io/panna/reference/calculate_spm_ratings_xgb.md),
[`fit_spm_model()`](https://peteowen1.github.io/panna/reference/fit_spm_model.md),
[`get_spm_feature_importance()`](https://peteowen1.github.io/panna/reference/get_spm_feature_importance.md)
