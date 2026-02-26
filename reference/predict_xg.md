# Predict xG for Shots

Applies trained xG model to predict goal probability for shots.

## Usage

``` r
predict_xg(xg_model, shot_features)
```

## Arguments

- xg_model:

  Fitted xG model from fit_xg_model()

- shot_features:

  Data frame with shot features (same format as training)

## Value

Vector of xG predictions (probabilities)

## Examples

``` r
if (FALSE) { # \dontrun{
xg_model <- fit_xg_model(training_shots)
new_shots <- prepare_shots_for_xg(new_shot_events)
xg_pred <- predict_xg(xg_model, new_shots)
} # }
```
