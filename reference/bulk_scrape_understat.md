# Bulk scrape Understat matches with auto-detection

Scrapes matches by ID range, auto-detecting league and season from
metadata. This is useful for scraping all matches across all leagues
without knowing which IDs belong to which league.

## Usage

``` r
bulk_scrape_understat(
  start_id,
  end_id,
  delay = 3,
  skip_cached = TRUE,
  verbose = TRUE
)
```

## Arguments

- start_id:

  Starting match ID

- end_id:

  Ending match ID (inclusive)

- delay:

  Seconds between requests (default 3)

- skip_cached:

  Skip already cached matches (default TRUE)

- verbose:

  Print progress messages (default TRUE)

## Value

Data frame with scraping results (match_id, league, season, status)

## Examples

``` r
if (FALSE) { # \dontrun{
# Scrape recent matches (2024-25 season IDs are ~27000-29000)
results <- bulk_scrape_understat(28900, 28999)

# Check results
table(results$league, results$status)
} # }
```
