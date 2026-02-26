# Calculate effective minute including stoppage time

Converts minute + added_time into a single continuous time value.
Second-half events are offset by first-half stoppage to create
continuous time.

## Usage

``` r
calculate_effective_minute(minute, added_time, first_half_stoppage = 0L)
```

## Arguments

- minute:

  Integer vector of base minutes

- added_time:

  Integer vector of added time (can be NA)

- first_half_stoppage:

  Integer, minutes of first-half stoppage (default 0)

## Value

Numeric vector of effective minutes

## Details

Examples with 3 mins first-half stoppage:

- First half minute 30 -\> 30

- First half 45+3 -\> 48

- Second half minute 46 -\> 49 (offset by 3)

- Second half 90+11 -\> 104 (90 + 11 + 3)
