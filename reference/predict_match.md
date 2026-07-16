# Predict Match Outcome Probabilities

Given fitted goals and outcome models, predicts P(H), P(D), P(A) and
expected goals for a set of matches.

## Usage

``` r
predict_match(
  goals_home_model,
  goals_away_model,
  outcome_model,
  X_goals,
  X_outcome
)
```

## Arguments

- goals_home_model:

  Fitted XGBoost Poisson model for home goals

- goals_away_model:

  Fitted XGBoost Poisson model for away goals

- outcome_model:

  Fitted XGBoost multinomial model

- X_goals:

  Feature matrix for goals models

- X_outcome:

  Feature matrix for outcome model (without goal predictions)

## Value

Data frame with pred_home_goals, pred_away_goals, prob_H, prob_D, prob_A

## See also

Other match prediction:
[`aggregate_lineup_ratings()`](https://peteowen1.github.io/panna/reference/aggregate_lineup_ratings.md),
[`aggregate_lineup_skills()`](https://peteowen1.github.io/panna/reference/aggregate_lineup_skills.md),
[`calibration_table()`](https://peteowen1.github.io/panna/reference/calibration_table.md),
[`compute_match_elos()`](https://peteowen1.github.io/panna/reference/compute_match_elos.md),
[`compute_multiclass_logloss()`](https://peteowen1.github.io/panna/reference/compute_multiclass_logloss.md),
[`compute_team_rolling_features()`](https://peteowen1.github.io/panna/reference/compute_team_rolling_features.md),
[`init_team_elos()`](https://peteowen1.github.io/panna/reference/init_team_elos.md),
[`update_elo()`](https://peteowen1.github.io/panna/reference/update_elo.md)
