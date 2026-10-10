# Add xG to SPADL Actions

Adds xG predictions to shot actions in SPADL data.

## Usage

``` r
add_xg_to_spadl(
  spadl_actions,
  xg_model,
  season = NULL,
  shot_lookup = NULL,
  events = NULL,
  foot_history = NULL
)
```

## Arguments

- spadl_actions:

  SPADL actions data frame

- xg_model:

  Fitted xG model

- season:

  Season label for these actions (e.g. "2025-2026", "2026", "2026
  Canada-Mexico-USA"). SPADL carries no season or date column, so a
  season-aware model cannot derive it and this must be supplied; the end
  year is read with
  [`extract_season_end_year()`](https://peteowen1.github.io/panna/reference/extract_season_end_year.md),
  exactly as training does. Required when the model's features include
  `season_num` - it aborts rather than score without it.

- shot_lookup:

  Optional data frame keyed by (`match_id`, `event_id`) carrying
  `body_part` and `situation` for shot events, e.g. `opta_shot_events`.
  Strongly recommended: SPADL's own `bodypart` is a stub that labels
  every shot "foot", so without this the header and footedness flags are
  dead and set pieces score as open play. Joined on `original_event_id`,
  the same key
  [`add_xgot_to_spadl()`](https://peteowen1.github.io/panna/reference/add_xgot_to_spadl.md)
  uses.

- events:

  Full Opta events for these matches. Needed only by a model that reads
  pre-shot context (xG v5: assist, possession, rebound, score); such a
  model aborts without it. See
  [`.shot_context()`](https://peteowen1.github.io/panna/reference/dot-shot_context.md).

- foot_history:

  Each shooter's earlier foot shots
  ([`.shot_foot_history()`](https://peteowen1.github.io/panna/reference/dot-shot_foot_history.md)).
  Needed only by a model that reads `foot_share`.

## Value

SPADL actions with xg column added for shots
