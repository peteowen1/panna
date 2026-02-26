# Minimum xG value (prevents 0 in log calculations)

Floor for xG predictions to prevent issues with log calculations and
overly confident predictions of 0 probability.

## Usage

``` r
XG_MIN
```

## Format

Numeric value: 0.01

## Examples

``` r
XG_MIN
#> [1] 0.01
```
