# Compute keeper shot-stopping (GSAA) from SPADL + lineups

Goals Saved Above Average = expected goals faced - goals conceded,
attributed to the conceding team's primary keeper. "Expected goals
faced" uses post-shot xGOT when available (what the keeper actually had
to stop), else pre-shot xG. Positive = saved more than expected.
Replaces the scale-free save_percentage.

## Usage

``` r
.compute_keeper_gsaa(spadl, lineups, by_match = FALSE)
```

## Arguments

- spadl:

  SPADL actions with xg (and optionally xgot), result, team_id.

- lineups:

  Lineups with position, team_id, match_id, minutes_played.

- by_match:

  Logical. One row per keeper-match if TRUE, else per keeper.

## Value

data.table keyed by `player_id` + `team_id` (and `match_id` when
`by_match = TRUE`) with gsaa, gsaa_per90, xgot_faced, goals_conceded; or
NULL if not computable.
