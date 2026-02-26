# Get cached match URLs from metadata

Reads all cached metadata files for a league-season and extracts the
match URLs. Useful for re-scraping or updating cached matches.

## Usage

``` r
get_cached_match_urls(league, season)
```

## Arguments

- league:

  League code (e.g., "ENG", "ESP")

- season:

  Season string (e.g., "2023-2024")

## Value

Character vector of match URLs

## Examples

``` r
if (FALSE) { # \dontrun{
urls <- get_cached_match_urls("ENG", "2023-2024")
length(urls)  # Number of cached matches
} # }
```
