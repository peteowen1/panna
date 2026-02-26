# Create Next xG Labels for EPV Model

Determines the xG of the next shot in each half for each action.
Positive xG = team's shot, Negative xG = opponent's shot, 0 = no more
shots. This provides more signal than binary goal outcomes.

## Usage

``` r
create_next_xg_labels(spadl_actions, xg_values = NULL)
```

## Arguments

- spadl_actions:

  SPADL actions with team_id, period_id, match_id

- xg_values:

  Optional named vector or data frame with xG values per action_id. If
  NULL, uses chain_xg from spadl_actions if available, otherwise
  estimates.

## Value

Data frame with next_xg_label column added
