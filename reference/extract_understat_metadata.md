# Extract match metadata from Understat page

Parses match info including teams, scores, and xG values. Note:
Understat uses a flat JSON structure for match_info.

## Usage

``` r
extract_understat_metadata(page, understat_id)
```

## Arguments

- page:

  Parsed HTML document

- understat_id:

  Understat match ID

## Value

Data frame with match metadata (single row)
