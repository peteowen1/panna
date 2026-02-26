# Scrape matches by Understat ID range

Iteratively scrapes matches within a given ID range. This is useful when
fixture lists cannot be obtained directly (since Understat loads them
via JS).

## Usage

``` r
scrape_understat_match_range(
  start_id,
  end_id,
  league,
  season,
  delay = 3,
  skip_invalid = TRUE
)
```

## Arguments

- start_id:

  Starting match ID

- end_id:

  Ending match ID (inclusive)

- league:

  League code for caching (e.g., "ENG")

- season:

  Season for caching (e.g., 2024)

- delay:

  Seconds between requests (default 3)

- skip_invalid:

  If TRUE (default), silently skip invalid match IDs

## Value

List of match results (metadata only, roster/shots require JS)

## Details

Understat match IDs are sequential integers. Recent seasons tend to have
higher IDs. For example, 2024-25 Premier League matches are in the
~28000 range.

## Examples

``` r
if (FALSE) { # \dontrun{
# Scrape 10 matches starting from ID 28900
results <- scrape_understat_match_range(28900, 28909, "ENG", 2024)

# Extract just the metadata
metadata <- dplyr::bind_rows(lapply(results, function(x) x$metadata))
} # }
```
