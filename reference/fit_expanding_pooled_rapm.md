# Fit a pooled RAPM restricted to seasons strictly before a cutoff year

Mirrors the production step-04 pooled RAPM fit (panna#87 bracketed
mini-CV lambda grid), but on an expanding-window ROW SUBSET of the
design matrix instead of the full history – the as-of deployment shape
recommended by FABLE-ASOF-EXPERIMENTS.md sec 4.

## Usage

``` r
fit_expanding_pooled_rapm(
  rapm_data,
  splint_season_map,
  cutoff_year,
  min_year = NULL,
  lambda_formula = function(n) 16.67 * n^(-0.58),
  nfolds = 5,
  seed = NULL
)
```

## Arguments

- rapm_data:

  The pooled `rapm_data` list from `04_rapm.rds$rapm_data`.

- splint_season_map:

  data.frame/data.table with `splint_id`, `season_end_year`.

- cutoff_year:

  Integer; only seasons `< cutoff_year` are used to train.

- min_year:

  Integer or `NULL` (default). When supplied, bounds the training window
  to `min_year <= season_end_year < cutoff_year` instead of the full
  expanding history (BOX-SCORE-VALUE-SPM-REDESIGN.md sec 2.1's windowed
  prior-free RAPM target, e.g. a 5-season window
  `min_year = cutoff_year - 5`). Passed straight through to
  [`.subset_rapm_data_expanding()`](https://peteowen1.github.io/panna/reference/dot-subset_rapm_data_expanding.md).

- lambda_formula:

  `function(n_obs)` giving the mini-CV grid center (default the panna#87
  sample-size formula, `16.67 * n_obs^-0.58`).

- nfolds:

  CV folds (default 5, matching the production pooled fit).

- seed:

  RNG seed for
  [`fit_rapm()`](https://peteowen1.github.io/panna/reference/fit_rapm.md)'s
  CV fold assignment, for reproducible per-cutoff-year fits. `NULL` = no
  explicit seed.

## Value

List: `ratings` (data.frame player_id/rapm/offense/defense),
`lambda_min`, `n_obs`, `cutoff_year`, `min_year` (the argument as
supplied, `NULL` for the unbounded expanding-window default). `NULL`
(with a warning) if fewer than 1000 valid observations remain (too few
prior seasons, or an empty/too-narrow window).
