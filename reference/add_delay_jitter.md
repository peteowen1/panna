# Add random jitter to delay

Returns the base delay plus random jitter to appear more human.

## Usage

``` r
add_delay_jitter(base_delay, jitter_pct = 0.3)
```

## Arguments

- base_delay:

  Base delay in seconds

- jitter_pct:

  Percentage of jitter (default 0.3 = +/- 30%)

## Value

Delay in seconds with jitter applied
