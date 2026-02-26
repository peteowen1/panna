# Suggest Available Seasons for an Opta League

Returns available seasons for a league, checking local data then
catalog. Useful for discovering what season format a competition uses
(e.g., "2024-2025" for leagues vs "2018 Russia" for World Cup).

## Usage

``` r
suggest_opta_seasons(league, table_type = "match_events", base_dir = NULL)
```

## Arguments

- league:

  League code (e.g., "World_Cup", "EPL", "AFCON"). Accepts both panna
  aliases (e.g., "ENG") and Opta codes.

- table_type:

  Table type to check (default: "match_events").

- base_dir:

  Opta data directory. If NULL, auto-detected.

## Value

Character vector of available seasons (most recent first), or empty.

## Examples

``` r
if (FALSE) { # \dontrun{
suggest_opta_seasons("World_Cup")
# [1] "2022 Qatar" "2018 Russia" "2014 Brazil" ...

suggest_opta_seasons("EPL")
# [1] "2025-2026" "2024-2025" "2023-2024" ...
} # }
```
