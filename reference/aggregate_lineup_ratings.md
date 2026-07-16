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

## See also

Other match prediction:
[`aggregate_lineup_skills()`](https://peteowen1.github.io/panna/reference/aggregate_lineup_skills.md),
[`calibration_table()`](https://peteowen1.github.io/panna/reference/calibration_table.md),
[`compute_match_elos()`](https://peteowen1.github.io/panna/reference/compute_match_elos.md),
[`compute_multiclass_logloss()`](https://peteowen1.github.io/panna/reference/compute_multiclass_logloss.md),
[`compute_team_rolling_features()`](https://peteowen1.github.io/panna/reference/compute_team_rolling_features.md),
[`init_team_elos()`](https://peteowen1.github.io/panna/reference/init_team_elos.md),
[`predict_match()`](https://peteowen1.github.io/panna/reference/predict_match.md),
[`update_elo()`](https://peteowen1.github.io/panna/reference/update_elo.md)
