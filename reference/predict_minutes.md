# Predict minutes for new rows (must have same feature columns as training)

Predict minutes for new rows (must have same feature columns as
training)

## Usage

``` r
predict_minutes(model, newdata)
```

## Arguments

- model:

  Output of
  [`fit_minutes_model()`](https://peteowen1.github.io/panna/reference/fit_minutes_model.md).

- newdata:

  Data.frame with feature columns.

## Value

Numeric vector of expected minutes per row.

## See also

Other expected minutes:
[`build_team_expected_minutes()`](https://peteowen1.github.io/panna/reference/build_team_expected_minutes.md),
[`classify_role()`](https://peteowen1.github.io/panna/reference/classify_role.md),
[`prepare_minutes_cache()`](https://peteowen1.github.io/panna/reference/prepare_minutes_cache.md),
[`query_minutes_features()`](https://peteowen1.github.io/panna/reference/query_minutes_features.md)
