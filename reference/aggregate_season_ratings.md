# Aggregate player game ratings to season level

Summarizes per-game ratings to one row per player per season, with total
and per-90 averages for all value metrics.

## Usage

``` r
aggregate_season_ratings(game_ratings, season_col = "season")
```

## Arguments

- game_ratings:

  Output of
  [`build_player_game_ratings`](https://peteowen1.github.io/panna/reference/build_player_game_ratings.md).

- season_col:

  Column name containing season identifier (default `"season"`). If not
  present, all data is treated as one season.

## Value

A data.table with one row per player (per season), containing summed
totals and minutes-weighted per-90 averages.
