# Initialize Team Elo Ratings

Creates a named vector of initial Elo ratings for all teams. Filters NA
team names defensively – they would otherwise create an NA-named entry
that `NA %in% names(elos)` returns TRUE for, opening the door to NA
cascades when bad upstream data sneaks through.

## Usage

``` r
init_team_elos(teams, initial_elo = 1500)
```

## Arguments

- teams:

  Character vector of team names

- initial_elo:

  Starting Elo rating (default 1500)

## Value

Named numeric vector of Elo ratings (one entry per non-NA team)
