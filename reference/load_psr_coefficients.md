# Load bundled PSR coefficients

Loads pre-trained PSR coefficient CSV files from the package's
`inst/extdata` directory.

## Usage

``` r
load_psr_coefficients(
  type = c("margin", "offense", "defense"),
  target = c("xg", "goals", "blend"),
  model = c("outfield", "gk")
)
```

## Arguments

- type:

  One of `"margin"`, `"offense"`, or `"defense"`.

- target:

  One of `"xg"` (default, xG differential), `"goals"` (goal
  differential), or `"blend"` (alpha\*xG + (1-alpha)\*goals; falls back
  to `"xg"` if the blend files are not yet generated).

- model:

  One of `"outfield"` (default) or `"gk"` (goalkeeper sub-model, trained
  on goal differential).

## Value

A data.frame with columns `stat_name`, `beta`, and optionally `sd`.
