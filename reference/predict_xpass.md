# Predict Pass Completion Probability

Applies trained xPass model to predict completion probability for
passes.

## Usage

``` r
predict_xpass(xpass_model, pass_features)
```

## Arguments

- xpass_model:

  Fitted xPass model from fit_xpass_model()

- pass_features:

  Data frame with pass features

## Value

Vector of xPass predictions (probabilities)

## Examples

``` r
if (FALSE) { # \dontrun{
xpass_model <- load_xpass_model()
pass_features <- prepare_passes_for_xpass(spadl_actions)
xpass_pred <- predict_xpass(xpass_model, pass_features)
} # }
```
