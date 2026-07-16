# Get default decay parameters for skill estimation

Returns sensible default exponential decay rates (lambda) per stat
category. Lambda controls how quickly old observations lose influence:
`weight = exp(-lambda * days_since_match)`.

## Usage

``` r
get_default_decay_params()
```

## Value

Named list with elements:

- rate:

  Lambda for per-90 rate stats (~8 month half-life)

- efficiency:

  Lambda for efficiency/accuracy stats (~1 year half-life)

- xmetrics:

  Lambda for xG/xA/xPass metrics (~8 month half-life)

- prior_90s:

  Gamma prior strength for rate stats, in equivalent 90s. Higher = more
  shrinkage toward position mean. Default 2.

- prior_attempts:

  Beta prior strength for efficiency stats, in equivalent attempts.
  Higher = more shrinkage. Default 50.

## Details

Half-life = ln(2) / lambda. Higher lambda = faster decay = more recency
bias.

## See also

Other estimated skills:
[`adjust_match_stats_for_context()`](https://peteowen1.github.io/panna/reference/adjust_match_stats_for_context.md),
[`backtest_skill_predictions()`](https://peteowen1.github.io/panna/reference/backtest_skill_predictions.md),
[`compute_position_multipliers()`](https://peteowen1.github.io/panna/reference/compute_position_multipliers.md),
[`estimate_player_skills()`](https://peteowen1.github.io/panna/reference/estimate_player_skills.md),
[`estimate_player_skills_at_date()`](https://peteowen1.github.io/panna/reference/estimate_player_skills_at_date.md),
[`inspect_skill()`](https://peteowen1.github.io/panna/reference/inspect_skill.md),
[`player_skill_profile()`](https://peteowen1.github.io/panna/reference/player_skill_profile.md)

## Examples

``` r
params <- get_default_decay_params()
# Half-life in days: log(2) / params$rate
```
