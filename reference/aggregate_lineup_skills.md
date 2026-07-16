# Aggregate Player Skills to Team-Level Features

For each match, takes the starting XI and their skill estimates, then
aggregates key skills to team level. Produces granular skill features
(e.g., team average shooting skill, team average tackling skill) that
give the XGBoost model richer signal beyond a single panna rating.

## Usage

``` r
aggregate_lineup_skills(
  lineups,
  skill_estimates,
  attacking_stats = NULL,
  defensive_stats = NULL
)
```

## Arguments

- lineups:

  Data frame of match lineups with player_name, team_name, team_position
  (home/away), position, is_starter columns

- skill_estimates:

  Data frame from
  [`estimate_player_skills()`](https://peteowen1.github.io/panna/reference/estimate_player_skills.md)
  with player_name and per-stat skill columns (e.g., goals_p90,
  tackles_won_p90)

- attacking_stats:

  Character vector of attacking skill columns to aggregate

- defensive_stats:

  Character vector of defensive skill columns to aggregate

## Value

Data frame with one row per match, team-level skill features

## See also

Other match prediction:
[`aggregate_lineup_ratings()`](https://peteowen1.github.io/panna/reference/aggregate_lineup_ratings.md),
[`calibration_table()`](https://peteowen1.github.io/panna/reference/calibration_table.md),
[`compute_match_elos()`](https://peteowen1.github.io/panna/reference/compute_match_elos.md),
[`compute_multiclass_logloss()`](https://peteowen1.github.io/panna/reference/compute_multiclass_logloss.md),
[`compute_team_rolling_features()`](https://peteowen1.github.io/panna/reference/compute_team_rolling_features.md),
[`init_team_elos()`](https://peteowen1.github.io/panna/reference/init_team_elos.md),
[`predict_match()`](https://peteowen1.github.io/panna/reference/predict_match.md),
[`update_elo()`](https://peteowen1.github.io/panna/reference/update_elo.md)
