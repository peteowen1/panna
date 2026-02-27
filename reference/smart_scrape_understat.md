# Smart scrape Understat with per-league tracking

Scrapes Understat matches using per-league max ID tracking. Each league
is scanned independently from its own max ID, which handles the
interleaved nature of Understat match IDs across leagues.

## Usage

``` r
smart_scrape_understat(
  manifest_path,
  leagues = c("RUS", "ENG", "ESP", "FRA", "ITA", "GER"),
  lookback = 20,
  max_misses = 25,
  delay = 3,
  verbose = TRUE
)
```

## Arguments

- manifest_path:

  Path to manifest parquet file

- leagues:

  Character vector of leagues to scrape (default: all 6)

- lookback:

  Number of IDs to look back from max (handles gaps)

- max_misses:

  Stop scanning a league after this many consecutive misses

- delay:

  Seconds between requests

- verbose:

  Print progress messages

## Value

Data frame with scraping results (match_id, league, status)

## Examples

``` r
if (FALSE) { # \dontrun{
# Run smart scraper
results <- smart_scrape_understat(
  manifest_path = "data/understat-manifest.parquet",
  lookback = 20,
  max_misses = 25,
  delay = 3
)

# Check results
table(results$league, results$status)
} # }
```
