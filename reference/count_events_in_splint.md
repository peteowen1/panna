# Count events within each splint

Counts events that occur IN each splint (between start and end
boundaries). Unlike count_events_before which gives cumulative counts,
this gives per-splint counts.

## Usage

``` r
count_events_in_splint(events, boundaries)
```

## Arguments

- events:

  Data frame with 'minute' (or 'effective_minute') and 'is_home' columns

- boundaries:

  Numeric vector of splint boundary minutes

## Value

List with 'home' and 'away' counts (vectors of length n_splints)
