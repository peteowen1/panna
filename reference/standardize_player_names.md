# Standardize player names

Cleans and standardizes player names for consistent matching across
datasets. Uses memoization to cache unique names for O(1) lookup on
repeated values.

## Usage

``` r
standardize_player_names(names)
```

## Arguments

- names:

  Character vector of player names

## Value

Character vector of standardized names
