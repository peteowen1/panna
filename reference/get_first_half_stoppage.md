# Get first-half stoppage time from events

Finds the maximum added_time for events at minute 45.

## Usage

``` r
get_first_half_stoppage(events)
```

## Arguments

- events:

  Data frame with minute and optionally added_time columns

## Value

Integer, first-half stoppage in minutes (0 if none)
