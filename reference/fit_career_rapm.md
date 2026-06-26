# Fit career-trait Panna (decay-weighted multi-season xRAPM)

Pools every splint across all seasons into one ridge plus-minus fit,
weighting each observation by exponential recency decay, and shrinks
toward the career-trait skill-SPM prior. Yields one rating per player as
of `reference_date` — the "how good is this player / next-game impact"
trait (parallel to EPR/PSR), distinct from the per-season `xrapm`
contribution.

## Usage

``` r
fit_career_rapm(
  splint_data,
  match_dates,
  skill_spm = NULL,
  halflife_days = 365,
  reference_date = NULL,
  min_minutes = 200,
  nfolds = 5,
  offense_prior = NULL,
  defense_prior = NULL,
  fixed_lambda = NULL,
  lambda_formula = NULL
)
```

## Arguments

- splint_data:

  Splint list (as in `cache-opta/03_splints.rds`); needs `splints` with
  `match_id`.

- match_dates:

  data.frame/data.table with `match_id` + `match_date` (e.g. from
  `opta_fixtures.parquet`); gives each splint its age for decay.

- skill_spm:

  Skills-pipeline SPM object (as in `cache-skills/03_skill_spm.rds`);
  its `offense_spm_ratings$offense_spm` and
  `defense_spm_ratings$defense_spm` (by `player_id`) are the prior.
  Ignored if `offense_prior`/`defense_prior` are supplied directly.

- halflife_days:

  Recency half-life in days: weight
  `= 0.5 ^ (age_days / halflife_days)`. Default 365 (~1 year) — tuned
  via `optimize_panna_decay` on held-out match prediction (365d was the
  best, monotone "shorter is better"; the objective is near-flat, like
  EPR's, so the exact value is non-critical and 365 matches the "best
  guess of next game" intent). 2026-06-09.

- reference_date:

  "As of" Date for ages; default = latest `match_date`.

- min_minutes:

  Minimum career minutes to be rated (else replacement pool).

- nfolds:

  CV folds for the ridge fit.

- offense_prior, defense_prior:

  Optional named (by `player_id`) prior vectors that override
  `skill_spm`.

- fixed_lambda:

  Optional single ridge lambda (skips `cv.glmnet`). Default `NULL` =
  cross-validated.

- lambda_formula:

  Optional `function(n_obs)` returning a lambda; used only when
  `fixed_lambda` is `NULL`. The as-of-date snapshot build passes the
  sample-size formula (`16.67 * n_obs^-0.58`) so each reference date
  gets a sample-appropriate lambda without re-running CV. `n_obs` is the
  count of valid (finite) splint observations actually fed to the fit.

## Value

List with `model` (the xRAPM fit), `ratings` (data.table:
`player_id, player_name, panna, panna_offense, panna_defense, total_minutes`),
`halflife_days`, and `reference_date`.
