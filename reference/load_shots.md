# Load Shots Data

Loads shot-level data with expected goals (xG) from pannadata.

## Usage

``` r
load_shots(league = NULL, season = NULL, source = c("remote", "local"))
```

## Arguments

- league:

  Optional league filter (e.g., "ENG", "ESP", "GER").

- season:

  Optional season filter (e.g., "2023-2024").

- source:

  Data source: "remote" (default) or "local".

## Value

Data frame of shot data.

## Examples

``` r
if (FALSE) { # \dontrun{
# Load all shots from remote
all_shots <- load_shots()

# Load Spain 2023-2024 shots
esp_shots <- load_shots(league = "ESP", season = "2023-2024")
} # }
```
