# Load Passing Data

Loads passing statistics from pannadata.

## Usage

``` r
load_passing(league = NULL, season = NULL, source = c("remote", "local"))
```

## Arguments

- league:

  Optional league filter (e.g., "ENG", "ESP", "GER").

- season:

  Optional season filter (e.g., "2023-2024").

- source:

  Data source: "remote" (default) or "local".

## Value

Data frame of passing statistics.

## Examples

``` r
if (FALSE) { # \dontrun{
# Load all passing data
all_passing <- load_passing()

# Load Italy 2024-2025 passing
ita_passing <- load_passing(league = "ITA", season = "2024-2025")
} # }
```
