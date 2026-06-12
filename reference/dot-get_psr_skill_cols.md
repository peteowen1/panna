# Get PSR skill feature column names

Returns the per-90 rate and efficiency columns used as PSR features.
This matches the feature set used in
[`fit_spm_opta()`](https://peteowen1.github.io/panna/reference/fit_spm_opta.md)
minus position dummies – the elastic net selects relevant features
automatically.

## Usage

``` r
.get_psr_skill_cols()
```

## Value

Character vector of column names
