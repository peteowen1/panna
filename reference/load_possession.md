# Load Possession Data

Loads possession statistics from pannadata.

## Usage

``` r
load_possession(league = NULL, season = NULL, source = c("remote", "local"))
```

## Arguments

- league:

  Optional league filter (e.g., "ENG", "ESP", "GER").

- season:

  Optional season filter (e.g., "2023-2024").

- source:

  Data source: "remote" (default) or "local".

## Value

Data frame of possession statistics.

## Examples

``` r
if (FALSE) { # \dontrun{
# Load all possession data
all_possession <- load_possession()

# Load England 2024-2025 possession stats
eng_poss <- load_possession(league = "ENG", season = "2024-2025")
} # }
```
