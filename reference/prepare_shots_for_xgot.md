# Prepare on-target shots for xGOT modeling

Filters to on-target shots within the complete goalmouth window, then
builds pre-shot features (shared with xG via .create_shot_features())
plus placement features (.create_placement_features()).

## Usage

``` r
prepare_shots_for_xgot(
  shot_events,
  min_season_end_year = XGOT_MIN_SEASON_END_YEAR
)
```

## Arguments

- shot_events:

  Data frame from load_opta_shot_events(); must include goalmouth_y /
  goalmouth_z (run pannadata backfill_goalmouth.py first).

- min_season_end_year:

  Earliest season end-year to keep (default 2021).

## Value

Data frame of features + target is_goal, ready for fit_xgot_model().
