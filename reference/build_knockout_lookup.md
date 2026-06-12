# Build the full-model knockout matchup lookup

Predicts every possible pairwise knockout matchup with the full goals +
outcome models, so the World Cup simulator can use full-fidelity
probabilities for knockout ties instead of a compressed Bradley-Terry
rating.

## Usage

``` r
build_knockout_lookup(
  match_dataset,
  goals_models,
  outcome_result,
  season = "2026 Canada-Mexico-USA",
  hosts = c("United States", "Canada", "Mexico"),
  verbose = TRUE
)
```

## Arguments

- match_dataset:

  The step-04 match dataset (has every team's WC2026 feature rows).

- goals_models:

  Step-05 goals models – with `$feature_cols` top-level and `$pooled` /
  `$international` sub-objects each holding `$home` / `$away`.

- outcome_result:

  Step-06 outcome models – with `$augmented_features` top-level and
  `$pooled` / `$international` sub-objects each holding `$model`.

- season:

  WC season string used to locate the team rows.

- hosts:

  Host nations that get `home_field` advantage.

- verbose:

  Print progress.

## Value

A list:

- probs:

  data.table keyed by `key = "t1||t2"` (t1 \< t2 alphabetically) with
  `p_t1`, `p_draw`, `p_t2`, `lambda_t1`, `lambda_t2`.

- lookup:

  environment hash: `lookup[[key]]` -\>
  `c(p_t1, p_draw, p_t2, lambda_t1, lambda_t2)` for O(1) access.

- team_elo:

  named numeric vector of each team's pre-tournament Elo.

## Details

Each matchup row is assembled from the two teams' feature vectors
(extracted from the WC2026 rows of the match dataset), with the
home/away diff columns recomputed and a host-aware `home_field`.
Predictions are symmetrized (both orientations averaged) so they do not
depend on listing order.

World Cup matchups are international, so each matchup is predicted as a
blend of the pooled and international-specialist models
(`MATCH_INTL_BLEND_WEIGHT` on the specialist).
