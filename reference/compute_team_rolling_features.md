# Compute Team Rolling Features

Calculates rolling averages of team performance metrics using strictly
lagged windows (no data leakage). Uses data.table frollmean + shift.

## Usage

``` r
compute_team_rolling_features(results, windows = c(5L, 10L, 20L))
```

## Arguments

- results:

  Data frame of match results with match_id, match_date, home_team,
  away_team, home_goals, away_goals, home_xg, away_xg

- windows:

  Rolling window sizes (default c(5, 10, 20))

## Value

Data frame with match_id and rolling features for home/away
