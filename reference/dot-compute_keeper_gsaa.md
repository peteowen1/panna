# Compute keeper shot-stopping (GSAA) from SPADL + lineups

Goals Saved Above Average = expected goals faced - goals conceded,
attributed to the keeper who was in goal when each shot was taken.
"Expected goals faced" uses post-shot xGOT when available (what the
keeper actually had to stop), else pre-shot xG. Positive = saved more
than expected. Replaces the scale-free save_percentage.

## Usage

``` r
.compute_keeper_gsaa(spadl, lineups, by_match = FALSE)
```

## Arguments

- spadl:

  SPADL actions with xg (and optionally xgot), result, team_id.
  `time_seconds` (match clock) and `period_id` place each shot in a
  keeper's span; `player_id` plus `action_type`/`opta_type_id` find
  substitute keepers.

- lineups:

  Lineups with position, team_id, match_id, minutes_played, and
  `sub_on_minute` / `sub_off_minute` for the spans.

- by_match:

  Logical. One row per keeper-match if TRUE, else per keeper.

## Value

data.table keyed by `player_id` + `team_id` (and `match_id` when
`by_match = TRUE`) with gsaa, gsaa_per90, xgot_faced, goals_conceded; or
NULL if not computable.

## Details

Each conceded shot goes to the keeper on the pitch at its time, from his
on-pitch span (kick-off, or `sub_on_minute`, to `sub_off_minute`; a red
card with no off-minute ends the span at `start + minutes_played` when
another keeper played). A keeper who came off the bench has
`position = "Substitute"` in the lineup, so he is recognised by
keeper-only events (Opta types 11, 41, 52, 53, 54, 59), by being a
keeper in the rest of the lineups, or by being the single substitute who
came on at the minute a keeper went off; a keeper who goes off with no
identifiable replacement triggers a warning naming the match, and the
later shots stay with the starter. Without `time_seconds` on the shots,
or where no identified keeper's span covers a shot, the shot falls back
to the team-match's keeper with most minutes (the old
one-keeper-per-match rule).
