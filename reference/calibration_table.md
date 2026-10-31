# Create Calibration Table

Groups predictions into bins and compares predicted vs actual
probabilities.

## Usage

``` r
calibration_table(y_true, prob_matrix, n_bins = 10L)
```

## Arguments

- y_true:

  Integer vector of true outcomes (0=H, 1=D, 2=A)

- prob_matrix:

  Matrix with 3 columns of predicted probabilities

- n_bins:

  Number of calibration bins (default 10)

## Value

A data.table with one row per outcome (Home, Draw, Away) and bin, so
`3 * n_bins` rows. Columns: `bin` (interval label), `bin_lower`,
`bin_upper`, `pred_mean`, `actual_mean`, `n`, `outcome`. A bin no
prediction falls into has `n = 0` and `NA` for `pred_mean` and
`actual_mean`. If any predicted probability for an outcome is `NA`, that
outcome gets one extra row with `bin = NA` and `n` set to the number of
such rows, so the bin counts always add up to the number of predictions.
Probabilities outside `[0, 1]` are an error.

## See also

Other match prediction:
[`aggregate_lineup_ratings()`](https://peteowen1.github.io/panna/reference/aggregate_lineup_ratings.md),
[`aggregate_lineup_skills()`](https://peteowen1.github.io/panna/reference/aggregate_lineup_skills.md),
[`compute_match_elos()`](https://peteowen1.github.io/panna/reference/compute_match_elos.md),
[`compute_multiclass_logloss()`](https://peteowen1.github.io/panna/reference/compute_multiclass_logloss.md),
[`compute_team_rolling_features()`](https://peteowen1.github.io/panna/reference/compute_team_rolling_features.md),
[`init_team_elos()`](https://peteowen1.github.io/panna/reference/init_team_elos.md),
[`predict_match()`](https://peteowen1.github.io/panna/reference/predict_match.md),
[`update_elo()`](https://peteowen1.github.io/panna/reference/update_elo.md)
