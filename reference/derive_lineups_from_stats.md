# Get match lineups from advanced stats (no scraping needed)

Derives lineup information from advanced match stats data. This avoids
rate limiting issues by using pre-scraped data. Can use any stat type
(summary, passing, defense, possession) - all contain the core
player/match info needed for lineups.

## Usage

``` r
derive_lineups_from_stats(stats_data)
```

## Arguments

- stats_data:

  Stats data from pannadata (already snake_case). Can be any stat type -
  passing/defense/possession have better historical coverage.

## Value

Data frame of match lineups derived from stats
