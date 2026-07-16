# Add win probability and WPA to SPADL data

Adds `wp` (model's possession-POV win probability at each action —
P(team that performed the action wins the match)) and `wpa` (change in
the acting team's win probability between the current and the next
event) columns.

## Usage

``` r
add_wp_vars(wp_features, wp_model)
```

## Arguments

- wp_features:

  SPADL features with WP model features. MUST contain:

  - `match_id` — match identifier for centering + shift bounds

  - `team_id` — acting team (load-bearing for the WPA POV pivot on
    `team_id_next`)

  - `is_home` — POV indicator (currently unused but reserved)

  - `wp_label` — last-action fallback target

  - plus the feature columns the model was trained on

  Missing `team_id` silently produces wrong WPA via the case_when on
  `team_id_next == team_id` — see WPA scale regression retro at
  `CLAUDE_TODO_WPA_SCALE_REGRESSION.md`.

- wp_model:

  Trained WP model from
  [`train_wp_model`](https://peteowen1.github.io/panna/reference/train_wp_model.md).
  Predictions are clamped to `[0, 1]` by
  [`predict_wp`](https://peteowen1.github.io/panna/reference/predict_wp.md).

## Value

The input data.table with added `wp` (possession-POV probability) and
`wpa` (acting-team-POV delta) columns.

## Details

WPA delta accounts for possession switches:

- Same team in possession at t+1 (`team_id_next == team_id`):
  `wpa = wp_next - wp` — both values are P(same team wins)

- Possession switched at t+1: `wp_next` is P(other team wins), so from
  the t-team's POV the post-event probability is `(1 - wp_next)`, giving
  `wpa = (1 - wp_next) - wp`

Pre-2026-05-29 implementation took raw `wp_next - wp` deltas which
silently inflated WPA ~30x once the model was retrained to
possession-POV (see `C:/dev/pannaverse/panna/debug/demo_wpa_logic.R` for
a worked example). Mirrors torpverse's `add_variables.R` case_when on
`team_id_next` vs `team_id_mdl`.

WPA is centered per-match (`wpa - mean(wpa, na.rm=TRUE)` by `match_id`)
to remove model-calibration bias.

## See also

Other win probability:
[`aggregate_player_game_wpa()`](https://peteowen1.github.io/panna/reference/aggregate_player_game_wpa.md),
[`assign_wpa_credit()`](https://peteowen1.github.io/panna/reference/assign_wpa_credit.md),
[`create_wp_features()`](https://peteowen1.github.io/panna/reference/create_wp_features.md),
[`load_wp_model()`](https://peteowen1.github.io/panna/reference/load_wp_model.md),
[`predict_wp()`](https://peteowen1.github.io/panna/reference/predict_wp.md),
[`save_wp_model()`](https://peteowen1.github.io/panna/reference/save_wp_model.md),
[`train_wp_model()`](https://peteowen1.github.io/panna/reference/train_wp_model.md)
