# Create win probability features from SPADL actions

Builds the game-state feature set at each action for WP model training
and prediction. Features capture score state, expected goals state, time
remaining, and team strength indicators.

## Usage

``` r
create_wp_features(spadl_with_epv, match_results = NULL, home_teams = NULL)
```

## Arguments

- spadl_with_epv:

  SPADL actions with EPV. Must contain: `match_id`, `team_id`,
  `time_seconds`, `period_id`.

- match_results:

  Data.frame with `match_id`, `home_team_id`, `away_team_id`,
  `home_goals`, `away_goals` for training labels. If NULL, labels are
  not added (prediction mode).

- home_teams:

  Optional data.frame with `match_id`, `home_team_id` to determine
  home/away. If NULL, derived from match_results.

## Value

A data.table with one row per action, containing WP features and
optionally training labels.
