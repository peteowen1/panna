# Scrape multiple Understat matches

Scrapes multiple matches with rate limiting and caching.

## Usage

``` r
scrape_understat_matches(
  understat_ids,
  league,
  season,
  delay_seconds = 3,
  skip_cached = TRUE,
  max_matches = NULL
)
```

## Arguments

- understat_ids:

  Vector of Understat match IDs

- league:

  League code

- season:

  Season

- delay_seconds:

  Delay between requests (default 3)

- skip_cached:

  Skip already cached matches (default TRUE)

- max_matches:

  Maximum matches to scrape (default NULL = all)

## Value

List with counts of scraped, cached, and failed matches
