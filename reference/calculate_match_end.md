# Calculate match end minute from events and/or shots

Determines the actual match end time based on the latest event or shot
observed. Uses continuous time (second half offset by first-half
stoppage). When both events and shots are provided, uses the maximum of
both.

## Usage

``` r
calculate_match_end(events, shots = NULL, default_end = 91)
```

## Arguments

- events:

  Data frame with minute and optionally added_time columns

- shots:

  Data frame with minute column (character, e.g., "90+4")

- default_end:

  Default match end if no data (default 91)

## Value

Numeric match end minute
