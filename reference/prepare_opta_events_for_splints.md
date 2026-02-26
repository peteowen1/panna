# Prepare Opta Events for Splint Creation

Converts Opta event data to the format expected by splint creation
functions. Opta events have: event_type (goal, substitution, red_card,
yellow_card), minute, second, team_id, player_id, etc.

## Usage

``` r
prepare_opta_events_for_splints(opta_events, match_results = NULL)
```

## Arguments

- opta_events:

  Data frame from load_opta_events()

- match_results:

  Data frame with home_team, away_team, team_id mapping

## Value

Data frame with columns: match_id, minute, added_time, event_type,
is_goal, is_sub, is_red_card, is_home, player_id, player_name
