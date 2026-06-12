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
