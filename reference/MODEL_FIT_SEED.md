# Seed for model fits that draw on R's random stream

xgboost 3.x's R `xgb.train` takes its internal seed from R's random
stream when `params` has none, and `xgb.cv` draws its folds from it, so
an unseeded retrain on identical data gives a different model
(panna#360). Each model fit sets this with
[`withr::local_seed()`](https://withr.r-lib.org/reference/with_seed.html),
which also restores the caller's stream.

## Usage

``` r
MODEL_FIT_SEED
```

## Format

An object of class `integer` of length 1.
