# Add value metric columns to splints

Joins per-splint player value metrics (EPV, WPA, PSV) to splint data.

## Usage

``` r
add_value_metrics_to_splints(
  splint_data,
  player_action_epv = NULL,
  match_action_wpa = NULL,
  player_game_psv = NULL
)
```

## Arguments

- splint_data:

  List with `splints` and `players` data.frames (from
  [`create_all_splints()`](https://peteowen1.github.io/panna/reference/create_all_splints.md)).
  `splints` must carry `start_minute` and `splint_id` when
  `player_action_epv` or `match_action_wpa` is supplied (the boundary
  cut needs them).

- player_action_epv:

  Per-action, per-credited-player EPV stream from
  `build_action_epv_credit()`
  (`data-raw/epv/02_calculate_player_epv.R`). Columns: `match_id`,
  `period_id`, `time_seconds`, `team_id`, `player_id`, `credit`. If
  NULL, `epv_home`/`epv_away` are not added.

- match_action_wpa:

  Per-action, home-perspective, UNcentered WP-delta stream
  (`data-raw/epv/06_calculate_wpa.R`). Columns: `match_id`, `period_id`,
  `time_seconds`, `wp_delta_home`. If NULL, `wpa_home`/`wpa_away` are
  not added.

- player_game_psv:

  Per-game PSV from
  [`calculate_psv()`](https://peteowen1.github.io/panna/reference/calculate_psv.md).
  If NULL, PSV columns are not added.

## Value

The `splint_data` list with additional columns on the `splints`
data.frame: `epv_home`/`epv_away` (per-splint sums – NOT zero-sum, both
teams accrue their own threat), `wpa_home`/`wpa_away` (per-splint sums,
EXACTLY zero-sum by construction: `wpa_away = -wpa_home`),
`psv_home`/`psv_away` (whole-match value prorated by splint duration).

## Details

EPV and WPA use TRUE per-splint attribution (FABLE-PRIOR-FIX-PLAN.md
D1/D2, Step 3): the per-action credit streams from the EPV/WPA pipelines
(Step 2) are cut at splint boundaries using the same `findInterval`
convention as the xG shot attribution
([`calculate_splint_npxgd_fast()`](https://peteowen1.github.io/panna/reference/calculate_splint_npxgd_fast.md),
R/splint_creation.R) – ties at a boundary timestamp go to the splint
STARTING there, and actions at/after the last boundary attribute to the
final splint. This replaces the old whole-match-value x
duration-proration join, under which the target could not vary within a
match (FABLE-PRIOR-FIX-PLAN.md C1: duration cancels exactly against the
per-90 conversion, so `whole_match_value * 90 / match_duration` is
constant for every splint of that match).

PSV has no per-action stream (D3: it's a bottom-up box-score value with
no per-splint count cache) and keeps the whole-match join + duration
proration.

## See also

Other panna ratings:
[`aggregate_season_ratings()`](https://peteowen1.github.io/panna/reference/aggregate_season_ratings.md),
[`extract_xrapm_ratings()`](https://peteowen1.github.io/panna/reference/extract_xrapm_ratings.md)
