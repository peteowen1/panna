# Compute Multi-Class Log Loss

Compute Multi-Class Log Loss

## Usage

``` r
compute_multiclass_logloss(y_true, prob_matrix, eps = 1e-15)
```

## Arguments

- y_true:

  Integer vector of true labels (0, 1, 2)

- prob_matrix:

  Matrix with 3 columns (P(0), P(1), P(2))

- eps:

  Clipping epsilon to avoid log(0) (default 1e-15)

## Value

Scalar log loss value

## See also

Other match prediction:
[`aggregate_lineup_ratings()`](https://peteowen1.github.io/panna/reference/aggregate_lineup_ratings.md),
[`aggregate_lineup_skills()`](https://peteowen1.github.io/panna/reference/aggregate_lineup_skills.md),
[`calibration_table()`](https://peteowen1.github.io/panna/reference/calibration_table.md),
[`compute_match_elos()`](https://peteowen1.github.io/panna/reference/compute_match_elos.md),
[`compute_team_rolling_features()`](https://peteowen1.github.io/panna/reference/compute_team_rolling_features.md),
[`init_team_elos()`](https://peteowen1.github.io/panna/reference/init_team_elos.md),
[`predict_match()`](https://peteowen1.github.io/panna/reference/predict_match.md),
[`update_elo()`](https://peteowen1.github.io/panna/reference/update_elo.md)
