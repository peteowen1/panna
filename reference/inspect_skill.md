# Inspect skill estimate breakdown for a single stat

Shows the full Bayesian decomposition for every player: raw totals,
career average, prior center, prior weight, decay-weighted evidence, and
the final skill estimate.

## Usage

``` r
inspect_skill(
  stat_name,
  match_stats,
  decay_params = NULL,
  target_date = Sys.Date()
)
```

## Arguments

- stat_name:

  Name of the stat (e.g. `"goals_p90"`).

- match_stats:

  A data.table from
  [`compute_match_level_opta_stats()`](https://peteowen1.github.io/panna/reference/compute_match_level_opta_stats.md).

- decay_params:

  Decay parameters list.

- target_date:

  Date to estimate skills as of. Default today.

## Value

A data.table sorted by skill estimate (descending), one row per player.

## See also

Other estimated skills:
[`adjust_match_stats_for_context()`](https://peteowen1.github.io/panna/reference/adjust_match_stats_for_context.md),
[`backtest_skill_predictions()`](https://peteowen1.github.io/panna/reference/backtest_skill_predictions.md),
[`compute_position_multipliers()`](https://peteowen1.github.io/panna/reference/compute_position_multipliers.md),
[`estimate_player_skills()`](https://peteowen1.github.io/panna/reference/estimate_player_skills.md),
[`estimate_player_skills_at_date()`](https://peteowen1.github.io/panna/reference/estimate_player_skills_at_date.md),
[`get_default_decay_params()`](https://peteowen1.github.io/panna/reference/get_default_decay_params.md),
[`player_skill_profile()`](https://peteowen1.github.io/panna/reference/player_skill_profile.md)
