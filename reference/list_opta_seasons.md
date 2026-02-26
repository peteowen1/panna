# List Available Opta Seasons

Returns available seasons for a given league in the Opta data.

## Usage

``` r
list_opta_seasons(league, source = c("catalog", "remote", "local"))
```

## Arguments

- league:

  League code (e.g., "ENG", "EPL", "ESP", "La_Liga").

- source:

  Data source: "catalog" (default) reads from downloaded catalog,
  "remote" is an alias for "catalog", "local" scans the local
  filesystem. Falls back to catalog if local directory doesn't exist.

## Value

Character vector of available seasons.

## Examples

``` r
if (FALSE) { # \dontrun{
list_opta_seasons("ENG")
list_opta_seasons("EPL", source = "local")
} # }
```
