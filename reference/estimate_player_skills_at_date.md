# Estimate player skills at a specific date

Computes current skill vectors for specified players at a given date.
Useful for match prediction where you need skill estimates for upcoming
fixtures rather than end-of-season snapshots.

## Usage

``` r
estimate_player_skills_at_date(
  match_stats,
  decay_params = NULL,
  player_ids = NULL,
  date = Sys.Date(),
  min_weighted_90s = 5
)
```

## Arguments

- match_stats:

  A data.table from
  [`compute_match_level_opta_stats()`](https://peteowen1.github.io/panna/reference/compute_match_level_opta_stats.md).

- decay_params:

  Decay parameters.

- player_ids:

  Character vector of player_ids to estimate. If NULL, estimates all
  players with data before the date.

- date:

  Date to estimate skills at.

- min_weighted_90s:

  Regression threshold.

## Value

A data.table with one row per player containing skill estimates.

## See also

Other estimated skills:
[`adjust_match_stats_for_context()`](https://peteowen1.github.io/panna/reference/adjust_match_stats_for_context.md),
[`backtest_skill_predictions()`](https://peteowen1.github.io/panna/reference/backtest_skill_predictions.md),
[`compute_position_multipliers()`](https://peteowen1.github.io/panna/reference/compute_position_multipliers.md),
[`estimate_player_skills()`](https://peteowen1.github.io/panna/reference/estimate_player_skills.md),
[`get_default_decay_params()`](https://peteowen1.github.io/panna/reference/get_default_decay_params.md),
[`inspect_skill()`](https://peteowen1.github.io/panna/reference/inspect_skill.md),
[`player_skill_profile()`](https://peteowen1.github.io/panna/reference/player_skill_profile.md)
