# Scrape single Understat match

Fetches and parses match data from Understat. Uses HTML page for
metadata and events, and the API endpoint for roster and shots data.

## Usage

``` r
scrape_understat_match(understat_id, league, season, save_cache = TRUE)
```

## Arguments

- understat_id:

  Understat match ID

- league:

  League code (for caching)

- season:

  Season (for caching)

- save_cache:

  Whether to save to cache (default TRUE)

## Value

List with metadata, events, roster, and shots data frames

## Examples

``` r
if (FALSE) { # \dontrun{
match_data <- scrape_understat_match(28988, "ENG", 2024)
print(match_data$metadata)   # Match info, xG totals
print(match_data$events)     # Goals, subs, cards timeline
print(match_data$roster)     # Player stats (xG, xA, etc.)
print(match_data$shots)      # Shot-level xG
} # }
```
