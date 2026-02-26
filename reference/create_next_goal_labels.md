# Create Next Goal Labels for EPV Model

Determines who scores the next goal in each half for each action.
Labels: 0 = possession team scores next, 1 = opponent scores next, 2 =
nobody scores

## Usage

``` r
create_next_goal_labels(spadl_actions)
```

## Arguments

- spadl_actions:

  SPADL actions with team_id, period_id, match_id

## Value

Data frame with next_goal_label column added
