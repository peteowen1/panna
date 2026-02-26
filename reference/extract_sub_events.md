# Extract substitution times from lineups

Uses pre-calculated on_minute from leading space detection in player
names. FBref encodes substitutes with leading spaces - subs come on for
the player directly above them in the same team.

## Usage

``` r
extract_sub_events(lineups)
```

## Arguments

- lineups:

  Lineups data with on_minute column

## Value

Numeric vector of substitution minutes
