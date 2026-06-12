# Optimise EPR decay via hold-out prediction MSE

Grid-search the decay parameter for
[`calculate_epr_regression()`](https://peteowen1.github.io/panna/reference/calculate_epr_regression.md)
by fitting on history before a hold-out window and measuring weighted
MSE on next-game per-90 EPV in that window.

## Usage

``` r
optimize_epr_decay(
  player_game_epv,
  ref_date,
  holdout_days = 60L,
  decay_grid = c(200, 300, 400, 500, 700, 1000),
  verbose = TRUE,
  ...
)
```

## Arguments

- player_game_epv:

  As in calculate_epr_regression.

- ref_date:

  Snapshot date (the "today" of the test).

- holdout_days:

  Width of the hold-out window in days back from ref_date.

- decay_grid:

  Numeric vector of decay values (days) to evaluate.

- verbose:

  If TRUE (default), print per-candidate timing + score.

- ...:

  Passed to calculate_epr_regression (e.g., alpha, prior_strength).

## Value

A data.table with one row per decay candidate plus the chosen decay.
