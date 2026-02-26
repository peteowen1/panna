# Aggregate Player EPV Metrics

Summarizes EPV by player, calculating total and per-90 metrics. Properly
attributes receiver credit to the actual receivers.

## Usage

``` r
aggregate_player_epv(spadl_with_epv, lineups = NULL, min_minutes = 450)
```

## Arguments

- spadl_with_epv:

  SPADL actions with EPV and credit columns

- lineups:

  Optional lineup data for minutes played

- min_minutes:

  Minimum minutes for inclusion (default 450)

## Value

Data frame with player EPV statistics:

- player_id, player_name, team_id

- n_actions, total_minutes (if available)

- epv_total: Total EPV contribution

- epv_p90: EPV per 90 minutes

- epv_as_actor: EPV from own actions (player_credit)

- epv_as_receiver: EPV from receiving (successful passes + turnovers
  won)

- epv_duel_blame: EPV from losing duels (negative, summed from
  opponent_credit)

- epv_passing, epv_shooting, epv_dribbling, epv_defending
