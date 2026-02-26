# Beta prior alpha for finishing modifier (shrinkage toward 1.0)

Pseudocount added to both goals and xG when calculating finishing
modifier. Formula: (goals + BETA_PRIOR_ALPHA) / (xG + BETA_PRIOR_ALPHA)
This shrinks extreme values toward 1.0.

## Usage

``` r
BETA_PRIOR_ALPHA
```

## Format

Integer value: 5

## Examples

``` r
BETA_PRIOR_ALPHA
#> [1] 5
```
