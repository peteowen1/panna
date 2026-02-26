# Prepare Opta Shot Events for Splint xG Calculation

Converts Opta shot event data to the format expected by splint xG
calculation. Note: Opta API does not provide xG values directly - they
must be calculated from x/y coordinates using an xG model, or goals can
be used as a proxy.

## Usage

``` r
prepare_opta_shots_for_splints(
  opta_shot_events,
  use_goals_as_xg = FALSE,
  match_results = NULL
)
```

## Arguments

- opta_shot_events:

  Data frame from load_opta_shot_events()

- use_goals_as_xg:

  Logical. If TRUE, use is_goal as xG (1 for goal, 0 otherwise). This is
  a fallback when no xG model is available. Default FALSE.

- match_results:

  Optional data frame with home_team, team_id mapping

## Value

Data frame with columns: match_id, minute, team, player_id, player_name,
xg, is_goal, is_penalty
