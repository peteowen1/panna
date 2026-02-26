# Aggregate player season stats

Combines match-level stats into season aggregates.

## Usage

``` r
aggregate_player_season_stats(match_stats, rate_cols = NULL, count_cols = NULL)
```

## Arguments

- match_stats:

  Data frame of match-level player stats

- rate_cols:

  Columns with rate statistics

- count_cols:

  Columns with counting statistics

## Value

Data frame with season-level player stats
