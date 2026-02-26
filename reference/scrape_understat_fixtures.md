# Scrape fixtures from Understat league page

Gets list of matches with Understat IDs for a league and season.

## Usage

``` r
scrape_understat_fixtures(league, season, completed_only = TRUE)
```

## Arguments

- league:

  League code (e.g., "ENG", "ESP")

- season:

  Season year (e.g., 2024 or "2024")

- completed_only:

  If TRUE, only return completed matches (default TRUE)

## Value

Data frame with match info including understat_id, or NULL if data
cannot be extracted (expected for current Understat site structure)

## Note

As of 2024, Understat loads fixture data via JavaScript. This function
will return NULL because the data is not in the initial HTML response.
Use
[`scrape_understat_match_range()`](https://peteowen1.github.io/panna/reference/scrape_understat_match_range.md)
as an alternative to scrape matches by ID range, or use
RSelenium/chromote for JS-rendered content.

## Examples

``` r
if (FALSE) { # \dontrun{
# This will likely return NULL due to JS-loaded content
fixtures <- scrape_understat_fixtures("ENG", 2024)

# Alternative: use match ID range
results <- scrape_understat_match_range(28900, 28910, "ENG", 2024)
} # }
```
