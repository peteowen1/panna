# Load Metadata

Loads match metadata from pannadata.

## Usage

``` r
load_metadata(league = NULL, season = NULL, source = c("remote", "local"))
```

## Arguments

- league:

  Optional league filter (e.g., "ENG", "ESP", "GER").

- season:

  Optional season filter (e.g., "2023-2024").

- source:

  Data source: "remote" (default) or "local".

## Value

Data frame of match metadata.

## Examples

``` r
if (FALSE) { # \dontrun{
# Load all match metadata
all_meta <- load_metadata()

# Load Germany 2024-2025 metadata
ger_meta <- load_metadata(league = "GER", season = "2024-2025")
} # }
```
