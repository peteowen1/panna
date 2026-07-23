# Calculate Player Skill Ratings (PSR)

Computes PSR for each player by applying pre-trained glmnet coefficients
to individual player skill values. PSR represents each player's
predicted contribution to xG/goal differential based on their skill
profile.

## Usage

``` r
calculate_psr(skills, coef_df, center = TRUE)
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

- center:

  Logical. If TRUE (default), subtract the league mean so PSR =
  contribution above average player.

## Value

A data.table with identity columns plus `psr_raw` and `psr`.

## See also

Other psr:
[`PSV_RELIABILITY_GD_SCALE`](https://peteowen1.github.io/panna/reference/PSV_RELIABILITY_GD_SCALE.md),
[`calculate_psv()`](https://peteowen1.github.io/panna/reference/calculate_psv.md),
[`calculate_psv_components()`](https://peteowen1.github.io/panna/reference/calculate_psv_components.md),
[`compute_player_psv()`](https://peteowen1.github.io/panna/reference/compute_player_psv.md),
[`default_stat_rating_params()`](https://peteowen1.github.io/panna/reference/default_stat_rating_params.md),
[`load_opta_psr_weekly()`](https://peteowen1.github.io/panna/reference/load_opta_psr_weekly.md),
[`load_psv_match_reliability()`](https://peteowen1.github.io/panna/reference/load_psv_match_reliability.md),
[`player_psr()`](https://peteowen1.github.io/panna/reference/player_psr.md),
[`soccer_position_map()`](https://peteowen1.github.io/panna/reference/soccer_position_map.md),
[`soccer_stat_rating_definitions()`](https://peteowen1.github.io/panna/reference/soccer_stat_rating_definitions.md),
[`stat_rating_names()`](https://peteowen1.github.io/panna/reference/stat_rating_names.md)
