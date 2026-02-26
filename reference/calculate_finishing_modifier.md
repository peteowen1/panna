# Calculate player finishing modifier

Calculates a player's finishing ability relative to xG. Used to adjust
xG predictions in splint analysis.

## Usage

``` r
calculate_finishing_modifier(shooting, min_shots = 20)
```

## Arguments

- shooting:

  Processed shooting data

- min_shots:

  Minimum shots required (default 20)

## Value

Data frame with finishing modifier per player
