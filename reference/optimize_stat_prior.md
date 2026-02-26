# Optimize Bayesian prior strength for a single stat

Finds the optimal prior strength (prior_90s for rate stats,
prior_attempts for efficiency stats) that minimizes next-match
prediction MSE using the Gamma-Poisson or Beta-Binomial posterior.

## Usage

``` r
optimize_stat_prior(
  match_stats = NULL,
  stat_name,
  lambda = 0.003,
  pos_mean = NULL,
  is_efficiency = FALSE,
  denom_col = NULL,
  prior_bounds = NULL,
  lambda_bounds = NULL,
  quantile_bounds = NULL,
  optimize_lambda = FALSE,
  optimize_quantile = FALSE,
  pos_multipliers = NULL,
  min_history = 5,
  sample_n = 500,
  seed = 42,
  precomputed = NULL
)
```

## Arguments

- match_stats:

  A data.table with match-level stats.

- stat_name:

  Name of the stat column to optimize.

- lambda:

  Decay rate to use. Default 0.003.

- pos_mean:

  Population mean for the stat (prior center). If NULL, computed as
  minutes-weighted mean from the data.

- is_efficiency:

  If TRUE, uses Beta-Binomial with attempt-weighting.

- denom_col:

  Denominator column for efficiency stats.

- prior_bounds:

  Numeric vector of length 2: search range for prior strength. Default
  c(0.1, 100) for rate stats, c(0.1, 500) for efficiency.

- lambda_bounds:

  Numeric vector of length 2: search range for decay lambda. Default
  c(0, 0.02). Only used when `optimize_lambda = TRUE`.

- quantile_bounds:

  Numeric vector of length 2: search range for prior center quantile.
  Default c(0.3, 0.7). Only used when `optimize_quantile = TRUE`.

- optimize_lambda:

  If TRUE, jointly optimizes the decay rate lambda alongside prior
  strength using L-BFGS-B.

- optimize_quantile:

  If TRUE, jointly optimizes the prior center quantile alongside prior
  strength.

- pos_multipliers:

  Named list of position multipliers from
  [`compute_position_multipliers()`](https://peteowen1.github.io/panna/reference/compute_position_multipliers.md).

- min_history:

  Minimum prior matches for a prediction to count.

- sample_n:

  Max number of players to sample (for speed).

- seed:

  Random seed for player sampling.

- precomputed:

  Precomputed player splits from
  [`optimize_all_priors()`](https://peteowen1.github.io/panna/reference/optimize_all_priors.md),
  or NULL to compute from scratch.

## Value

A list with `stat`, `optimal_prior`, `mse`, and `n_predictions`.
