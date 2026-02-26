# Scrape match URLs from fixtures page

Fetches a league's fixtures page and extracts all match URLs.

## Usage

``` r
scrape_fixtures(league, season, completed_only = TRUE)
```

## Arguments

- league:

  League code (e.g., "ENG", "ESP")

- season:

  Season string (e.g., "2024-2025")

- completed_only:

  If TRUE, only return matches with scores (default TRUE)

## Value

Data frame with match_url, home_team, away_team, date columns

## Examples

``` r
if (FALSE) { # \dontrun{
fixtures <- scrape_fixtures("ENG", "2024-2025")
head(fixtures)
} # }
```
