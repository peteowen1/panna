# Simplify match positions to 4 groups

Maps Opta match positions (Goalkeeper, Defender, Wing Back, Midfielder,
Defensive Midfielder, Attacking Midfielder, Striker, Substitute) to
GK/DEF/MID/FWD.

## Usage

``` r
.simplify_position(pos)
```

## Arguments

- pos:

  Character vector of position strings.

## Value

Character vector with simplified positions (NA for unrecognized).
