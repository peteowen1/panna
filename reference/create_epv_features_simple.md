# Create Simple EPV Features

Builds a 15-feature set for EPV prediction: spatial location, movement,
time remaining, previous action context, action type, result, and league
identity. Designed for the xG-method simple model which prioritises
spatial signal while allowing league-specific adjustments.

## Usage

``` r
create_epv_features_simple(spadl_actions, league = NULL)
```

## Arguments

- spadl_actions:

  SPADL actions data frame

- league:

  League code (e.g., "ENG"). If NULL, uses `league` column from
  spadl_actions if present, otherwise defaults to 0 (unknown).

## Value

Data frame with 15 EPV features plus match_id and action_id
