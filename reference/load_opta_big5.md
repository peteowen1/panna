# Load All Opta Data for Big 5 Leagues

Convenience function to load Opta stats for all Big 5 European leagues.

## Usage

``` r
load_opta_big5(season = NULL, columns = NULL)
```

## Arguments

- season:

  Optional season filter. If NULL, loads all available seasons.

- columns:

  Optional character vector of columns to select.

## Value

Data frame with league column added.

## Examples

``` r
if (FALSE) { # \dontrun{
# Load all Big 5 data (warning: large!)
big5 <- load_opta_big5()

# Load specific season across all leagues
big5_2122 <- load_opta_big5(season = "2021-2022")
} # }
```
