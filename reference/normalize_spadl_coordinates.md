# Normalize SPADL Coordinates

WARNING: This function uses a heuristic that doesn't work well with
Opta's team-relative coordinate system. For EPV modeling, use
normalize_direction=FALSE (the default) to preserve Opta's native
coordinates where each team's actions are from their own perspective.

## Usage

``` r
normalize_spadl_coordinates(dt)
```

## Arguments

- dt:

  Data.table in SPADL format (modified in place)

## Value

Data.table with normalized coordinates (same object, modified)

## Details

Attempts to flip coordinates so all teams attack toward x=100. Uses mean
position per team/period as a heuristic, but this fails when both teams
have similar mean positions or when coordinates are team-relative.
