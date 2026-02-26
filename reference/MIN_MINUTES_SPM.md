# Default minimum minutes for SPM/player stats functions

Minimum minutes threshold used by player stats aggregation functions and
SPM model training. Higher than RAPM threshold because box score stats
need more sample size for stability.

## Usage

``` r
MIN_MINUTES_SPM
```

## Format

Integer value: 450

## Examples

``` r
MIN_MINUTES_SPM
#> [1] 450
```
