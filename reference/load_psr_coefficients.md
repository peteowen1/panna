# Load bundled PSR coefficients

Loads pre-trained PSR coefficient CSV files from the package's
`inst/extdata` directory.

## Usage

``` r
load_psr_coefficients(
  type = c("margin", "offense", "defense"),
  target = c("xg", "goals")
)
```

## Arguments

- type:

  One of `"margin"`, `"offense"`, or `"defense"`.

- target:

  One of `"xg"` (default, xG differential) or `"goals"` (goal
  differential).

## Value

A data.frame with columns `stat_name`, `beta`, and optionally `sd`.
