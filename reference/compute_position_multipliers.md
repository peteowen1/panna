# Compute position-specific multipliers for prior centers

For each stat, computes the ratio of position-specific weighted average
to the overall weighted average: `multiplier = pos_avg / global_avg`.
This allows the prior center to be position-specific while keeping the
global optimization.

## Usage

``` r
compute_position_multipliers(match_stats, stat_cols = NULL)
```

## Arguments

- match_stats:

  A data.table with match-level stats and a `position` column.

- stat_cols:

  Character vector of stat columns. If NULL, auto-detects.

## Value

A named list where each element is a named numeric vector of length 4
(GK, DEF, MID, FWD) giving the multiplier for that stat and position.

## See also

Other estimated skills:
[`adjust_match_stats_for_context()`](https://peteowen1.github.io/panna/reference/adjust_match_stats_for_context.md),
[`backtest_skill_predictions()`](https://peteowen1.github.io/panna/reference/backtest_skill_predictions.md),
[`estimate_player_skills()`](https://peteowen1.github.io/panna/reference/estimate_player_skills.md),
[`estimate_player_skills_at_date()`](https://peteowen1.github.io/panna/reference/estimate_player_skills_at_date.md),
[`get_default_decay_params()`](https://peteowen1.github.io/panna/reference/get_default_decay_params.md),
[`inspect_skill()`](https://peteowen1.github.io/panna/reference/inspect_skill.md),
[`player_skill_profile()`](https://peteowen1.github.io/panna/reference/player_skill_profile.md)
