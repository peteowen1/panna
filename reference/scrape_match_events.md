# Scrape match events timeline from FBref page

Parses the events wrap section to extract goals, cards, and
substitutions with precise minute information.

## Usage

``` r
scrape_match_events(page, match_url)
```

## Arguments

- page:

  Parsed HTML document

- match_url:

  Original match URL for ID extraction

## Value

Data frame with event timeline, or NULL if no events found
