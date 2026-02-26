# Download and Cache Opta Data Catalog

Loads the opta-catalog.json file, checking session cache first, then
local file, then downloading from GitHub releases.

## Usage

``` r
download_opta_catalog(repo = "peteowen1/pannadata", tag = "opta-latest")
```

## Arguments

- repo:

  GitHub repository (default: "peteowen1/pannadata").

- tag:

  Release tag (default: "opta-latest").

## Value

List with catalog data (competitions, panna_aliases).
