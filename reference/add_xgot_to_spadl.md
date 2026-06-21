# Add xGOT to SPADL Actions

Adds post-shot xG to shot actions. xGOT is defined only for ON-TARGET
shots, so this needs the goal-mouth crossing point, which SPADL drops -
it is joined back from `goalmouth_lookup` via the preserved
`original_event_id`. Assignment: on-target + coords -\> model prediction
on-target, no coords -\> NA (surfaced, never imputed) off-target -\> 0
(cannot score) non-shot / unmatched -\> NA

## Usage

``` r
add_xgot_to_spadl(spadl_actions, xgot_model, goalmouth_lookup)
```

## Arguments

- spadl_actions:

  SPADL actions data frame (must carry `original_event_id` and
  `match_id`).

- xgot_model:

  Fitted xGOT model.

- goalmouth_lookup:

  Data frame keyed by (`match_id`, `event_id`) with `type_id`,
  `goalmouth_y`, `goalmouth_z`, and `situation` for shot events - e.g.
  from match_events / opta_shot_events. `situation` is required to avoid
  train/serve skew (the model trained on real situations); without it,
  set-piece/corner/free-kick shots are scored as open-play.

## Value

SPADL actions with an `xgot` column added.
