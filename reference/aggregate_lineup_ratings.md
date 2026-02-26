# Aggregate Player Ratings to Team Level

For a given match, takes the starting XI from lineups and joins to
seasonal player ratings (xRAPM/SPM/RAPM). Computes team-level summary
statistics including sum, mean, max, min, stdev, goalkeeper, and
positional group averages.

## Usage

``` r
aggregate_lineup_ratings(
  lineups,
  ratings,
  season_end_year,
  prev_season_decay = 0.8
)
```

## Arguments

- lineups:

  Data frame of match lineups with player_name, team_name, team_position
  (home/away), position, is_starter columns

- ratings:

  Data frame of seasonal player ratings with player_name,
  season_end_year, panna, offense, defense, spm columns

- season_end_year:

  Numeric season end year for rating lookup

- prev_season_decay:

  Decay factor for previous season fallback (default 0.8)

## Value

Data frame with one row per match, team-level rating features
