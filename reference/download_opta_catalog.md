# Download and Cache Opta Data Catalog

Loads the opta-catalog.json file, checking session cache first, then
local file (with TTL freshness check), then downloading from GitHub
releases.

## Usage

``` r
download_opta_catalog(
  repo = "peteowen1/pannadata",
  tag = "opta-latest",
  max_age_hours = getOption("panna.opta_catalog_ttl_hours", 6)
)
```

## Arguments

- repo:

  GitHub repository (default: "peteowen1/pannadata").

- tag:

  Release tag (default: "opta-latest").

- max_age_hours:

  Freshness window for the local catalog cache. If the local file is
  older than this (mtime-based), it's treated as stale and
  re-downloaded. Default 6 hours, override globally via
  `options(panna.opta_catalog_ttl_hours = N)`. Set `Inf` to disable the
  TTL (legacy behavior – trust local forever).

## Value

List with catalog data (competitions, panna_aliases).
