# Extract match events with timing

Parses goals, substitutions, and red cards from event data with their
minutes.

## Usage

``` r
extract_match_events(events, match_id)
```

## Arguments

- events:

  Processed events data frame for a single match

- match_id:

  Match identifier to filter for

## Value

Data frame of events sorted by minute
