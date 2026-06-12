# Tune the career-Panna decay half-life on held-out match prediction

Temporal hold-out (mirrors `optimize_epr_decay`): fit career-Panna on
splints before `train_end = ref_date - holdout_days`, then predict the
held-out splints' xG-difference target and pick the half-life that
minimises weighted hold-out MSE. One shared design matrix is built once
and row-subset so train/hold-out columns stay aligned (so
[`predict()`](https://rdrr.io/r/stats/predict.html) is valid).

## Usage

``` r
optimize_panna_decay(
  splint_data,
  match_dates,
  skill_spm,
  halflife_grid = c(180, 365, 545, 730, 1095, 1460),
  ref_date = NULL,
  holdout_days = 150L,
  min_minutes = 200,
  nfolds = 5
)
```

## Arguments

- splint_data, match_dates, skill_spm:

  As in
  [`fit_career_rapm`](https://peteowen1.github.io/panna/reference/fit_career_rapm.md).

- halflife_grid:

  Half-lives (days) to evaluate.

- ref_date:

  "Today" of the test (Date). Default = latest match_date.

- holdout_days:

  Width of the hold-out window (days back from ref_date).

- min_minutes, nfolds:

  As in
  [`fit_career_rapm`](https://peteowen1.github.io/panna/reference/fit_career_rapm.md).

## Value

List: `results` (data.table halflife_days/holdout_wmse, sorted),
`best_halflife`, `ref_date`, `train_end`, `n_train`, `n_holdout`.
