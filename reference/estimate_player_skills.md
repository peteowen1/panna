# Estimate player skills using Bayesian conjugate priors with decay weighting

For each player, estimates "true skill" at a point in time using all
matches before `target_date`. Uses conjugate Bayesian updating with
exponential time decay:

## Usage

``` r
estimate_player_skills(
  match_stats,
  decay_params = NULL,
  target_date = NULL,
  min_weighted_90s = 5,
  stat_cols = NULL
)
```

## Arguments

- match_stats:

  A data.table from
  [`compute_match_level_opta_stats()`](https://peteowen1.github.io/panna/reference/compute_match_level_opta_stats.md).
  Must contain: match_date, player_id, player_name, total_minutes,
  position, and the stat columns to estimate.

- decay_params:

  Named list of decay rates and prior strengths. Output of
  [`get_default_decay_params()`](https://peteowen1.github.io/panna/reference/get_default_decay_params.md)
  or custom. Supports per-stat lambda overrides (named by stat column).
  Prior strength via `prior_90s` (Gamma, default 10) and
  `prior_attempts` (Beta, default 50).

- target_date:

  Date object. Only matches before this date are used. If NULL, uses all
  available data.

- min_weighted_90s:

  Not used for shrinkage (handled by Bayesian prior). Retained for
  backward compatibility.

- stat_cols:

  Character vector of stat columns to estimate. If NULL, auto-detects
  all \_p90 columns plus efficiency columns.

## Value

A data.table with one row per player containing estimated skill values
for each stat, plus player_id, player_name, position, and weighted_90s
(effective sample size in decay-weighted 90-minute units).

## Details

**Rate stats (per-90):** Gamma-Poisson model. Raw event counts are
back-calculated from `stat_p90 * (minutes/90)`, then decay-weighted. The
Gamma prior is centered on the position mean with strength controlled by
`prior_90s`. Posterior mean is in per-90 units.

**Efficiency stats (proportions):** Beta-Binomial model. Successes are
back-calculated from `proportion * attempts`, then decay-weighted. The
Beta prior is centered on the position mean with strength controlled by
`prior_attempts`.

Shrinkage toward position means happens naturally through the prior —
players with little data stay close to the prior, while players with
abundant data are driven by their observations.
