# Minimum weight threshold for duration-based weighting

Floor value for weights to prevent division by very small numbers in
RAPM weighting. Splints are weighted by minutes/90, with this as the
minimum.

## Usage

``` r
MIN_WEIGHT_DURATION
```

## Format

Numeric value: 0.01

## Examples

``` r
MIN_WEIGHT_DURATION
#> [1] 0.01
```
