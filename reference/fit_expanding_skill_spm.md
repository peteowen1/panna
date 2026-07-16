# Fit expanding-window skill-SPM O/D models for one reference year

Step B of the as-of fix: skill features are filtered to seasons strictly
before `cutoff_year` (an honest, point-in-time feature set), the
most-recent-per-player slice is taken (mirrors
`data-raw/estimated-skills/03_skill_spm.R`), and offense/defense
elastic-net + XGBoost SPM models are fit against the matching
[`fit_expanding_pooled_rapm()`](https://peteowen1.github.io/panna/reference/fit_expanding_pooled_rapm.md)
target – so NEITHER the features NOR the RAPM target the SPM predicts
can see season `cutoff_year` or later. Column sets come from the shared
[`.skill_spm_offense_cols()`](https://peteowen1.github.io/panna/reference/dot-skill_spm_offense_cols.md)
/
[`.skill_spm_defense_cols()`](https://peteowen1.github.io/panna/reference/dot-skill_spm_defense_cols.md)
/
[`.skill_spm_defense_constraints()`](https://peteowen1.github.io/panna/reference/dot-skill_spm_defense_constraints.md)
(the same definitions the all-history fit in `03_skill_spm.R` section 10
uses, so the two can never drift apart).

## Usage

``` r
fit_expanding_skill_spm(
  skill_features,
  pooled_rapm_ratings,
  cutoff_year,
  nfolds = 5
)
```

## Arguments

- skill_features:

  `02_skill_features.rds` (one row per player-season, needs `player_id`,
  `season_end_year`, `total_minutes`).

- pooled_rapm_ratings:

  Output of `fit_expanding_pooled_rapm()$ratings` for the SAME
  `cutoff_year` (needs `player_id`, `offense`, `defense`).

- cutoff_year:

  Integer; skill features from seasons `< cutoff_year` only.

- nfolds:

  CV folds for both glmnet and xgboost (default 5).

## Value

List with `offense_spm_glmnet`, `offense_spm_xgb`, `defense_spm_glmnet`,
`defense_spm_xgb`, `offense_spm_ratings`, `defense_spm_ratings` (same
shape as `03_skill_spm.rds`), `cutoff_year`, `n_train`. `NULL` (with a
warning) if fewer than 100 players are available to train on (e.g. the
earliest season, no prior data).
