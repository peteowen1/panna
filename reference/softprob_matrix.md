# Reshape a multi:softprob prediction into one row per observation

`xgboost >= 2.0` returns an `n x n_class` matrix from
[`predict()`](https://rdrr.io/r/stats/predict.html); older versions
return a flat, ROW-major vector. Reshaping the flat form with
`byrow = FALSE` silently scrambles classes across observations, and the
obvious guard does not catch it – a column-major reshape of a row-major
softprob vector can still produce rows summing to one. Every call site
goes through this helper rather than reshaping inline, so the xgboost
return contract is interpreted in exactly one place.

## Usage

``` r
softprob_matrix(probs, n_rows, n_class = 3L)
```

## Arguments

- probs:

  Raw [`predict()`](https://rdrr.io/r/stats/predict.html) output from a
  `multi:softprob` model.

- n_rows:

  Number of observations predicted.

- n_class:

  Number of classes (default 3: home / draw / away).

## Value

An `n_rows` x `n_class` numeric matrix.
