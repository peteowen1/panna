# Update Elo Ratings After a Match

Updates Elo ratings for home and away teams based on match result. Uses
standard Elo formula with configurable K-factor and home advantage.

## Usage

``` r
update_elo(
  home_elo,
  away_elo,
  home_goals,
  away_goals,
  k = 20,
  home_advantage = 65
)
```

## Arguments

- home_elo:

  Current home team Elo

- away_elo:

  Current away team Elo

- home_goals:

  Goals scored by home team

- away_goals:

  Goals scored by away team

- k:

  K-factor controlling update magnitude (default 20)

- home_advantage:

  Home advantage in Elo points (default 65)

## Value

Named list with new_home_elo, new_away_elo
