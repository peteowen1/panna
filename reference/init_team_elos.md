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

## See also

Other match prediction:
[`aggregate_lineup_ratings()`](https://peteowen1.github.io/panna/reference/aggregate_lineup_ratings.md),
[`aggregate_lineup_skills()`](https://peteowen1.github.io/panna/reference/aggregate_lineup_skills.md),
[`calibration_table()`](https://peteowen1.github.io/panna/reference/calibration_table.md),
[`compute_match_elos()`](https://peteowen1.github.io/panna/reference/compute_match_elos.md),
[`compute_multiclass_logloss()`](https://peteowen1.github.io/panna/reference/compute_multiclass_logloss.md),
[`compute_team_rolling_features()`](https://peteowen1.github.io/panna/reference/compute_team_rolling_features.md),
[`predict_match()`](https://peteowen1.github.io/panna/reference/predict_match.md),
[`update_elo()`](https://peteowen1.github.io/panna/reference/update_elo.md)
