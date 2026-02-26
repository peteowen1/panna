# Load Keeper Data

Loads goalkeeper statistics from pannadata.

## Usage

``` r
load_keeper(league = NULL, season = NULL, source = c("remote", "local"))
```

## Arguments

- league:

  Optional league filter (e.g., "ENG", "ESP", "GER").

- season:

  Optional season filter (e.g., "2023-2024").

- source:

  Data source: "remote" (default) or "local".

## Value

Data frame of goalkeeper statistics.

## Examples

``` r
if (FALSE) { # \dontrun{
# Load all goalkeeper data
all_keepers <- load_keeper()

# Load Spain 2024-2025 keeper stats
esp_keepers <- load_keeper(league = "ESP", season = "2024-2025")
} # }
```
