# Extract Season from Match Date

Extract Per-Player On/Off Times from Raw Opta Match Events

## Usage

``` r
extract_player_timing_from_events(match_events)
```

## Arguments

- match_events:

  Raw Opta match-events data frame with columns `match_id`, `type_id`,
  `period_id`, `team_id`, `player_id`, `minute`, `second`,
  `qualifier_json`.

## Value

Data frame with columns `match_id`, `player_id`, `team_id`,
`is_starter`, `on_minute`, `off_minute`. Bench players who never came on
are omitted. Returns empty data frame if input is empty or missing
required columns.

## Details

Derives `on_minute` and `off_minute` for every player in every match
directly from Opta event data – no reliance on lineup minute counts.
Uses second-level precision throughout.

Sources:

- Starting XI: `type_id == 34` (formation/squad set), qualifier 30 =
  player IDs, qualifier 131 = position number (1-11 = starter, 0 =
  bench).

- Sub on: `type_id == 19` (Player On).

- Sub off: `type_id == 18` (Player Off).

- Red card off: `type_id == 17` (Card) with qualifier 33 (straight red)
  or 32 (second yellow).

- Match end: `type_id == 30` with `period_id == 2` (used as default
  off_minute for finishers who never came off).

Why this beats lineups: Opta records `minutes_played = 90` for
unsubstituted finishers regardless of stoppage time, and rounds sub
timing to whole minutes. Chains carry the real second-precision times.

## See also

Other rapm:
[`assert_prior_free_target()`](https://peteowen1.github.io/panna/reference/assert_prior_free_target.md),
[`create_all_splints()`](https://peteowen1.github.io/panna/reference/create_all_splints.md),
[`create_rapm_design_matrix()`](https://peteowen1.github.io/panna/reference/create_rapm_design_matrix.md),
[`extract_period_end_times()`](https://peteowen1.github.io/panna/reference/extract_period_end_times.md),
[`extract_rapm_ratings()`](https://peteowen1.github.io/panna/reference/extract_rapm_ratings.md),
[`fit_rapm()`](https://peteowen1.github.io/panna/reference/fit_rapm.md),
[`fit_rapm_with_prior()`](https://peteowen1.github.io/panna/reference/fit_rapm_with_prior.md)
