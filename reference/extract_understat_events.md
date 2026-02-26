# Extract timeline events from Understat match page

Parses the HTML timeline to extract goals, substitutions, and cards.
This data IS available in the initial HTML (unlike roster/shots).

## Usage

``` r
extract_understat_events(page, understat_id)
```

## Arguments

- page:

  Parsed HTML document

- understat_id:

  Understat match ID

## Value

Data frame with events (goals, subs, cards), or NULL if not found
