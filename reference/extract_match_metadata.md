# Extract match metadata from FBref page

Parses scorebox to get teams, score, date, manager, captain, venue,
attendance, officials, and formations.

## Usage

``` r
extract_match_metadata(page, match_url)
```

## Arguments

- page:

  Parsed HTML document

- match_url:

  Original match URL for ID extraction

## Value

Data frame with match metadata (single row)
