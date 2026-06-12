# Extra-time seconds (120 minutes)

Duration including two 15-minute extra-time periods, in seconds. WP time
features use this denominator only for matches that actually reached
extra time — a fixed 5400 cap clamps every ET action to time_remaining
== 0, telling the model the match is over for the full 30 min of ET and
inflating per-event WPA in knockout matches.

## Usage

``` r
EXTRA_TIME_SECONDS
```

## Format

Numeric value: 7200
