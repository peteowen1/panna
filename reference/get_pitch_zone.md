# Determine Pitch Zone

Assigns a zone ID based on pitch location. Uses a 3x6 grid (18 zones).

## Usage

``` r
get_pitch_zone(x, y)
```

## Arguments

- x:

  X coordinate (0-100)

- y:

  Y coordinate (0-100)

## Value

Integer zone ID (1-18)

## Examples

``` r
if (FALSE) { # \dontrun{
get_pitch_zone(90, 20)   # Attacking third, left
} # }
```
