# Safe division handling division by zero

Safe division handling division by zero

## Usage

``` r
safe_divide(x, y, default = 0)
```

## Arguments

- x:

  Numerator

- y:

  Denominator

- default:

  Value to return when denominator is zero (default: 0)

## Value

x / y, with Inf/NaN from division-by-zero replaced by default. Input NAs
are preserved.

## Examples

``` r
safe_divide(10, 2)
#> [1] 5
safe_divide(10, 0)          # 0
#> [1] 0
safe_divide(10, 0, default = NA_real_)
#> [1] NA
```
