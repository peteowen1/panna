# Find the next event after each action via non-equi rolling join

Core helper for label creation. For each action, finds the first
matching event (goal, shot, etc.) that occurs after it in the same
match-period. Uses data.table non-equi join for O(n log n) performance.

## Usage

``` r
.find_next_event(dt, events_dt)
```

## Arguments

- dt:

  data.table of SPADL actions (must have match_id, period_id,
  time_seconds, action_id, team_id)

- events_dt:

  data.table of target events. Must have columns: match_id, period_id,
  event_time, event_team

## Value

dt with next_event_team column added (NA if no subsequent event)
