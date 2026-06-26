# Build the per-contest feature tables for one league's events (memory-safe)

Orders the event stream ONCE and extracts all five (small) contest
tables, so a caller can loop leagues and discard raw events between
iterations.

## Usage

``` r
compute_all_duel_preps(events)
```

## Arguments

- events:

  Full per-league Opta events.

## Value

Named list of five finalized feature tables.
