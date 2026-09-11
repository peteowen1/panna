# Build a pooled-only, two-legged pairwise knockout lookup for one UEFA cup

Predicts every possible league-phase-participant matchup with the full
goals + outcome models, two legs each (both hosting orientations, kept
separate – see file header point 3). Companion to
[`build_knockout_lookup()`](https://peteowen1.github.io/panna/reference/build_knockout_lookup.md)
for club cup competitions.

## Usage

``` r
build_cup_pairwise_lookup(
  match_dataset,
  goals_models,
  outcome_result,
  league,
  season,
  as_of = Sys.Date(),
  verbose = TRUE
)
```

## Arguments

- match_dataset:

  The step-04 match dataset.

- goals_models:

  Step-05 goals models (`$feature_cols` top-level, `$pooled$home` /
  `$pooled$away`).

- outcome_result:

  Step-06 outcome models (`$augmented_features` top-level,
  `$pooled$model`).

- league:

  One of `"UCL"`, `"UEL"`, `"UECL"`.

- season:

  Season label matching `match_dataset$season` for `league`.

- as_of:

  Reference date for "current state" (default: today). Exposed for tests
  and reproducible snapshots.

- verbose:

  Print progress.

## Value

A list:

- probs:

  data.table, one row per unordered team pair, with
  `leg1_home_goals`/`leg1_away_goals`/`leg1_pH`/`leg1_pD`/`leg1_pA` (t1
  hosts) and the `leg2_*` mirror (t2 hosts).

- team_as_of:

  named list, team -\> Date of the row its features were read from
  (freshness diagnostic).

- n_teams:

  league-phase team count (36 for the current UEFA format).
