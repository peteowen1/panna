# Safe division handling division by zero

Safe division handling division by zero

## Usage

``` r
safe_divide(x, y, default = NA_real_)
```

## Arguments

- x:

  Numerator

- y:

  Denominator

- default:

  Value to return when denominator is zero (default: NA_real\_)

## Value

x / y, with Inf/NaN from division-by-zero replaced by default. Input NAs
are preserved.

## Examples

``` r
safe_divide(10, 2)
#> [1] 5
safe_divide(10, 0)          # NA (unknown)
#> [1] NA
safe_divide(10, 0, default = 0)
#> [1] 0
```
