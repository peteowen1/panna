# Load Events Data

Loads match event data (goals, substitutions, cards, etc.) from
pannadata.

## Usage

``` r
load_events(league = NULL, season = NULL, source = c("remote", "local"))
```

## Arguments

- league:

  Optional league filter (e.g., "ENG", "ESP", "GER").

- season:

  Optional season filter (e.g., "2023-2024").

- source:

  Data source: "remote" (default) or "local".

## Value

Data frame of match events.

## Examples

``` r
if (FALSE) { # \dontrun{
# Load all events from remote
all_events <- load_events()

# Load England 2024-2025 events
eng_events <- load_events(league = "ENG", season = "2024-2025")
} # }
```
