# Count events before each boundary

Counts home/away events occurring before each splint boundary. Used for
tracking cumulative goals, red cards, etc.

## Usage

``` r
count_events_before(events, boundaries)
```

## Arguments

- events:

  Data frame with 'minute' (or 'effective_minute') and 'is_home' columns

- boundaries:

  Numeric vector of splint boundary minutes

## Value

List with 'home' and 'away' counts (vectors same length as boundaries
minus 1)
