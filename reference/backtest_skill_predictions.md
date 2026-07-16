# Backtest skill predictions against actual match performance

For each player-match in the data, computes the Bayesian skill estimate
using all prior matches (with decay weighting), then compares to actual
performance. This is equivalent to computing weekly skill estimates and
evaluating how well they predict the next match.

## Usage

``` r
backtest_skill_predictions(
  match_stats,
  decay_params = NULL,
  stat_cols = NULL,
  min_history = 5,
  sample_n = NULL,
  seed = 42
)
```

## Arguments

- match_stats:

  A data.table from
  [`compute_match_level_opta_stats()`](https://peteowen1.github.io/panna/reference/compute_match_level_opta_stats.md).

- decay_params:

  Decay parameters (with optional `stat_priors`).

- stat_cols:

  Character vector of stats to evaluate. If NULL, uses a default set of
  key stats.

- min_history:

  Minimum prior matches before predictions count (default 5).

- sample_n:

  Max players to sample for speed (default NULL = all).

- seed:

  Random seed for sampling.

## Value

A list with:

- predictions:

  data.table of individual predictions: player_id, match_date, stat,
  predicted, actual, minutes

- accuracy:

  data.table of per-stat accuracy metrics: RMSE, improvement vs simple
  average, improvement vs last match

## See also

Other estimated skills:
[`adjust_match_stats_for_context()`](https://peteowen1.github.io/panna/reference/adjust_match_stats_for_context.md),
[`compute_position_multipliers()`](https://peteowen1.github.io/panna/reference/compute_position_multipliers.md),
[`estimate_player_skills()`](https://peteowen1.github.io/panna/reference/estimate_player_skills.md),
[`estimate_player_skills_at_date()`](https://peteowen1.github.io/panna/reference/estimate_player_skills_at_date.md),
[`get_default_decay_params()`](https://peteowen1.github.io/panna/reference/get_default_decay_params.md),
[`inspect_skill()`](https://peteowen1.github.io/panna/reference/inspect_skill.md),
[`player_skill_profile()`](https://peteowen1.github.io/panna/reference/player_skill_profile.md)
