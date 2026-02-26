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
