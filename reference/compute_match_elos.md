# Compute Elo Ratings for All Matches

Iterates through matches chronologically and computes Elo ratings.
Returns match-level Elo features (pre-match ratings for each team).

## Usage

``` r
compute_match_elos(results, k = 20, home_advantage = 65, initial_elo = 1500)
```

## Arguments

- results:

  Data frame with match_date, home_team, away_team, home_goals,
  away_goals columns, sorted by date

- k:

  K-factor (default 20)

- home_advantage:

  Home advantage in Elo points (default 65)

- initial_elo:

  Starting Elo (default 1500)

## Value

Data frame with match_id, home_elo, away_elo, elo_diff columns
