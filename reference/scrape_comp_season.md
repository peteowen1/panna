# Scrape a competition-season

Scrapes all matches for a competition-season, either from cache or
FBref. Handles fixture fetching, caching logic, and progress reporting.

## Usage

``` r
scrape_comp_season(
  comp,
  season,
  table_types,
  delay,
  force_rescrape,
  max_matches = Inf
)
```

## Arguments

- comp:

  Competition code (e.g., "ENG", "UCL")

- season:

  Season string (e.g., "2023-2024")

- table_types:

  Character vector of table types to scrape

- delay:

  Seconds between requests

- force_rescrape:

  If TRUE, ignore cache and rescrape all

- max_matches:

  Maximum matches to scrape (default Inf)

## Value

Number of matches scraped (for tracking session totals)

## Examples

``` r
if (FALSE) { # \dontrun{
n <- scrape_comp_season("ENG", "2023-2024",
                        table_types = c("summary", "events"),
                        delay = 5, force_rescrape = FALSE)
} # }
```
