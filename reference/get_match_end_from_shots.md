# Get match end from shots data

Uses the last shot to determine match end time.

## Usage

``` r
get_match_end_from_shots(shots, first_half_stoppage = 0L)
```

## Arguments

- shots:

  Data frame with minute column (character, e.g., "90+4")

- first_half_stoppage:

  First half stoppage to offset second half

## Value

Numeric match end minute (effective time)
