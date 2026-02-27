# Calculate Action EPV Values

Computes EPV for each action. Supports two methods:

- "goal": EPV = P(team_scores) - P(opponent_scores), bounded -1 to +1

- "xg": EPV = expected xG of next shot (already signed)

## Usage

``` r
calculate_action_epv(spadl_actions, features, epv_model, xg_model = NULL)
```

## Arguments

- spadl_actions:

  SPADL actions data frame (with team_id, possession_change)

- features:

  EPV features from create_epv_features()

- epv_model:

  Fitted EPV model from fit_epv_model()

- xg_model:

  Optional pre-trained xG model from fit_xg_model(). If NULL, attempts
  to load from pannadata/data/opta/models/xg_model.rds. Falls back to
  position-based estimate if no model available.

## Value

SPADL actions with EPV columns added:

- epv: EPV at this state

- epv_delta: Change in EPV (with perspective handling)

- xg: For shots, the xG value (from model or estimated)

- For "goal" method: p_team_scores, p_opponent_scores, p_nobody_scores

- For "xg" method: expected_xg

## Details

Handles possession changes by flipping perspective:

- Same team: delta = EPV_after - EPV_before

- Team change: delta = (-EPV_after) - EPV_before
