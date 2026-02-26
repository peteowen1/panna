# Count events by splint boundary

Core logic for counting home/away events relative to splint boundaries.

## Usage

``` r
.count_events(events, boundaries, type = c("before", "within"))
```

## Arguments

- events:

  Data frame with 'minute' (or 'effective_minute') and 'is_home' columns

- boundaries:

  Numeric vector of splint boundary minutes

- type:

  Either "before" (cumulative before each boundary start) or "within"
  (events within each splint interval)

## Value

List with 'home' and 'away' counts (vectors of length n_splints)
