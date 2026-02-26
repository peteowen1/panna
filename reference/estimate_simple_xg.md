# Estimate Simple xG from Position

Simple xG estimation based on distance and angle to goal. Used as
fallback when no xG model is available. Calibrated to produce ~11% mean
xG (matching real shot distributions):

- 6 yard box (dist ~6): xG ~ 0.35-0.40

- Penalty spot (dist ~12): xG ~ 0.15-0.20

- Edge of box (dist ~18): xG ~ 0.06-0.10

- Long range (dist ~30): xG ~ 0.02-0.03

## Usage

``` r
estimate_simple_xg(x, y)
```

## Arguments

- x:

  X coordinate (0-100 scale, attacking right)

- y:

  Y coordinate (0-100 scale)

## Value

Estimated xG values
