# Calculate blended SPM ratings from Elastic Net and XGBoost

Combines predictions from both model types with configurable weighting.
The blend can improve robustness by capturing both linear (Elastic Net)
and non-linear (XGBoost) relationships between box scores and RAPM.

## Usage

``` r
calculate_spm_blend(
  player_features,
  model_glmnet,
  model_xgb,
  weight_glmnet = 0.5
)
```

## Arguments

- player_features:

  Data frame of player features

- model_glmnet:

  Fitted Elastic Net SPM model from fit_spm_model

- model_xgb:

  Fitted XGBoost SPM model from fit_spm_xgb

- weight_glmnet:

  Weight for Elastic Net predictions (default 0.5)

## Value

Data frame with blended SPM ratings plus individual model predictions
