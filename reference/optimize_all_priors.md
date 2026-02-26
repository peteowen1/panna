# Optimize prior strengths for all stats

Finds the optimal Bayesian prior strength per stat. Rate stats get
per-stat `prior_90s`, efficiency stats get per-stat `prior_attempts`.
Results are stored in `decay_params$stat_priors`.

## Usage

``` r
optimize_all_priors(
  match_stats,
  decay_params = NULL,
  stat_tiers = NULL,
  optimize_lambda = TRUE,
  optimize_quantile = FALSE,
  sample_n = 500,
  n_cores = 1,
  verbose = TRUE
)
```

## Arguments

- match_stats:

  A data.table from
  [`compute_match_level_opta_stats()`](https://peteowen1.github.io/panna/reference/compute_match_level_opta_stats.md).

- decay_params:

  Base decay parameters (provides lambda values).

- stat_tiers:

  Output of
  [`get_stat_tiers()`](https://peteowen1.github.io/panna/reference/get_stat_tiers.md),
  or NULL for defaults.

- optimize_lambda:

  If TRUE, jointly optimizes per-stat decay lambda.

- optimize_quantile:

  If TRUE, jointly optimizes per-stat prior center quantile.

- sample_n:

  Max players to sample per stat (default 500).

- n_cores:

  Number of cores for parallel optimization. Default 1 (sequential).

- verbose:

  Print progress.

## Value

Updated decay_params with `stat_priors` element: a named numeric vector
mapping stat names to optimal prior strengths.
