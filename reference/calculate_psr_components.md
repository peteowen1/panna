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

  **`sd` is deliberately the TEAM-SUM standard deviation from training,
  not a player-population sd (panna#167).** `07_train_psr_model.R`
  regresses match outcome on team-summed skill features (\\X\_{team,j} =
  \sum\_{11 players} skill_j\\), standardized by that sum's own sd.
  Since \\\partial X\_{team,j} / \partial(\text{one player's raw value})
  = 1\\ exactly, the chain rule gives \\\partial(\text{predicted
  margin}) / \partial(\text{player's raw value}\_j) = \beta_j /
  sd\_{team,j}\\ — which is exactly this function's
  `raw\_value / sd * beta` formula. This is the mathematically correct
  divisor for "marginal team-outcome effect of fielding a player with
  this stat profile" (the metric's documented purpose — see DECISIONS.md
  2026-07-20). Dividing by a player-population sd instead would answer a
  different, undefined question with a beta that was never fit for that
  scale, and was investigated and rejected as a "fix" — see panna#167
  and `pannaverse/docs/plans/FABLE-167-PSV-PSR-SD-INVESTIGATION.md` for
  the full derivation, a face-validity audit (high-touch players like
  Busquets/ Kimmich/Casemiro score correctly despite their signature
  stats carrying 15-21x team-sum/player-sd ratios), and a collinearity
  diagnostic confirming those extreme ratios track feature collinearity
  in the team-sum training data, not a scale-mismatch defect.

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
