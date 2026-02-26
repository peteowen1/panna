# Validate season input

Checks that seasons are in valid "YYYY-YYYY" format and within the
available data range. The minimum year depends on the data source: FBref
starts at 2017, Opta at 2013.

## Usage

``` r
validate_seasons(seasons, min_year = 2017, source_name = "FBref")
```

## Arguments

- seasons:

  Character vector of seasons in format "YYYY-YYYY" (e.g., "2023-2024")

- min_year:

  Minimum start year allowed (default 2017 for FBref)

- source_name:

  Name of the data source for error messages (default "FBref")

## Value

TRUE if valid, otherwise throws an error

## Examples

``` r
validate_seasons("2023-2024")
#> [1] TRUE
validate_seasons(c("2022-2023", "2023-2024"))
#> [1] TRUE
validate_seasons("2013-2014", min_year = 2013, source_name = "Opta")
#> [1] TRUE
```
