# Calculate PSR with Offensive/Defensive Decomposition

Computes the margin-based PSR (best single predictor of match outcomes),
then decomposes it into offensive (OSR) and defensive (DSR) components
using separately trained coefficient models. The decomposition uses an
additive shift so that `osr + dsr = psr` exactly.

## Usage

``` r
calculate_psr_components(
  skills,
  coef_df,
  osr_coef_df,
  dsr_coef_df,
  center = TRUE
)
```

## Arguments

- skills:

  A data.table/data.frame with player skill estimates, containing
  identity columns (`player_id`, `player_name`) and numeric skill
  columns matching the `stat_name` values in `coef_df`.

- coef_df:

  A data.frame with columns `stat_name` and `beta`. If an `sd` column is
  present, each skill is divided by its SD before multiplying by beta
  (i.e. the coefficients are on the standardized scale).

- osr_coef_df:

  Coefficient data.frame for the offensive model (same format as
  `coef_df`: columns `stat_name`, `beta`, optionally `sd`).

- dsr_coef_df:

  Coefficient data.frame for the defensive model.

- center:

  Logical. If TRUE (default), subtract the league mean so PSR =
  contribution above average player.

## Value

A data.table with columns: identity columns, `psr_raw`, `psr`, `osr`,
`dsr`.
