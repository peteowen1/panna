# Scrape full Understat season

Scrapes all matches for a league-season, first fetching fixtures.

## Usage

``` r
scrape_understat_season(
  league,
  season,
  delay_seconds = 3,
  skip_cached = TRUE,
  max_matches = NULL
)
```

## Arguments

- league:

  League code (e.g., "ENG")

- season:

  Season year (e.g., 2024)

- delay_seconds:

  Delay between requests (default 3)

- skip_cached:

  Skip already cached matches (default TRUE)

- max_matches:

  Maximum matches to scrape (default NULL = all)

## Value

List with counts of scraped, cached, and failed matches

## Examples

``` r
if (FALSE) { # \dontrun{
# Scrape all 2024 Premier League matches from Understat
scrape_understat_season("ENG", 2024)

# Scrape with limit for testing
scrape_understat_season("ENG", 2024, max_matches = 5)
} # }
```
