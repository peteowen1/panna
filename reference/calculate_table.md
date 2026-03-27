# Calculate League Table from Results

Calculate League Table from Results

## Usage

``` r
calculate_table(matches, teams, points_win = 3L, points_draw = 1L)
```

## Arguments

- matches:

  Data frame with home, away, home_goals, away_goals

- teams:

  Character vector of all teams

- points_win:

  Points for a win

- points_draw:

  Points for a draw

## Value

Data frame with team, played, won, drawn, lost, gf, ga, gd, points
