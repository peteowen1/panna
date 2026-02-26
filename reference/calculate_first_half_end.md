# Calculate first-half end minute from events and/or shots

Determines when first half actually ended based on stoppage time events
or shots. Uses full minutes: 45+3 -\> first half ends at 48.5 Falls back
to events only if shots is NULL.

## Usage

``` r
calculate_first_half_end(events, shots = NULL, default_end = 46)
```

## Arguments

- events:

  Data frame with minute and optionally added_time columns

- shots:

  Data frame with minute column (character, e.g., "45+3")

- default_end:

  Default first-half end if no stoppage data (default 46)

## Value

Numeric first-half end minute
