# Load Defense Data

Loads defensive statistics from pannadata.

## Usage

``` r
load_defense(league = NULL, season = NULL, source = c("remote", "local"))
```

## Arguments

- league:

  Optional league filter (e.g., "ENG", "ESP", "GER").

- season:

  Optional season filter (e.g., "2023-2024").

- source:

  Data source: "remote" (default) or "local".

## Value

Data frame of defensive statistics.

## Examples

``` r
if (FALSE) { # \dontrun{
# Load all defensive data
all_defense <- load_defense()

# Load France 2024-2025 defense stats
fra_defense <- load_defense(league = "FRA", season = "2024-2025")
} # }
```
