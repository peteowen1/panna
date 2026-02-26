# Get first-half stoppage from shots data

Parses shot minute strings to find max added_time at minute 45.

## Usage

``` r
get_first_half_stoppage_from_shots(shots)
```

## Arguments

- shots:

  Data frame with minute column (character, e.g., "45+3")

## Value

Integer, first-half stoppage in minutes (0 if none)
