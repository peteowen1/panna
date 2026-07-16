# Compute Team Rolling Features

Calculates rolling averages of team performance metrics using strictly
lagged windows (no data leakage). Uses data.table frollmean + shift.

## Usage

``` r
compute_team_rolling_features(results, windows = c(5L, 10L, 20L))
```

## Arguments

- results:

  Data frame of match results with match_id, match_date, home_team,
  away_team, home_goals, away_goals, home_xg, away_xg

- windows:

  Rolling window sizes (default c(5, 10, 20))

## Value

Data frame with match_id and rolling features for home/away

## See also

Other match prediction:
[`aggregate_lineup_ratings()`](https://peteowen1.github.io/panna/reference/aggregate_lineup_ratings.md),
[`aggregate_lineup_skills()`](https://peteowen1.github.io/panna/reference/aggregate_lineup_skills.md),
[`calibration_table()`](https://peteowen1.github.io/panna/reference/calibration_table.md),
[`compute_match_elos()`](https://peteowen1.github.io/panna/reference/compute_match_elos.md),
[`compute_multiclass_logloss()`](https://peteowen1.github.io/panna/reference/compute_multiclass_logloss.md),
[`init_team_elos()`](https://peteowen1.github.io/panna/reference/init_team_elos.md),
[`predict_match()`](https://peteowen1.github.io/panna/reference/predict_match.md),
[`update_elo()`](https://peteowen1.github.io/panna/reference/update_elo.md)
