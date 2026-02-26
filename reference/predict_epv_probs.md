# Predict EPV Values

Gets predictions from EPV model. For "goal" method returns multinomial
probabilities, for "xg" method returns expected next xG value.

## Usage

``` r
predict_epv_probs(model, features)
```

## Arguments

- model:

  EPV model from fit_epv_model()

- features:

  EPV features

## Value

For "goal": data frame with p_team_scores, p_opponent_scores,
p_nobody_scores For "xg": data frame with expected_xg
