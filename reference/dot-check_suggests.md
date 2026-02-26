# Check that a suggested package is installed

Throws an informative error if a package listed in Suggests is missing.

## Usage

``` r
.check_suggests(pkg, reason = NULL)
```

## Arguments

- pkg:

  Package name (character)

- reason:

  Brief reason why the package is needed

## Value

TRUE invisibly if package is available
