# Grouped cross-validation folds

Assigns whole GROUPS (not individual rows) to folds, so rows that share
a group id always land in the same fold. Used to keep a match and its
[`mirror_match_rows`](https://peteowen1.github.io/panna/reference/mirror_match_rows.md)
orientation-flipped twin together during CV early-stopping — otherwise a
match's mirror can land in a different fold than the original, letting
the same match inform both sides of a fold split (leakage-audit finding,
2026-08-27).

## Usage

``` r
.grouped_cv_folds(group_ids, nfolds, seed = 1234L)
```

## Arguments

- group_ids:

  Vector, one entry per row of the training data (e.g. `match_id`); rows
  sharing a value are always co-assigned. Must not contain `NA` (a row
  with an unknown group can't be assigned a fold).

- nfolds:

  Number of folds. Reduced to the number of distinct groups if there are
  fewer groups than folds (an `xgb.cv` fold with no rows in it does not
  error, so this would otherwise silently shrink CV to fewer effective
  folds without warning).

- seed:

  RNG seed for the fold assignment (reproducible).

## Value

A list of length `nfolds` (as possibly reduced above), each element a
vector of row indices forming that fold's TEST set — the format
`xgboost::xgb.cv(folds = ...)` expects.
