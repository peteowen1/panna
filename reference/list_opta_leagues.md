# List Available Opta Leagues

Returns a data frame of all competitions available in the Opta data,
with metadata including name, country, type, and tier.

## Usage

``` r
list_opta_leagues(
  type = NULL,
  tier = NULL,
  source = c("catalog", "remote", "local")
)
```

## Arguments

- type:

  Optional filter: "league", "cup", "domestic_cup", "international".

- tier:

  Optional numeric filter for competition tier (1 = top tier).

- source:

  Data source: "catalog" (default) downloads the catalog, "remote" is an
  alias for "catalog", "local" scans the local filesystem.

## Value

Data frame with columns: code, name, country, type, tier, n_seasons,
n_matches, panna_alias.

## Examples

``` r
if (FALSE) { # \dontrun{
# All competitions
list_opta_leagues()

# Top-tier leagues only
list_opta_leagues(tier = 1)

# Just cups
list_opta_leagues(type = "cup")

# Scan local filesystem (no download)
list_opta_leagues(source = "local")
} # }
```
