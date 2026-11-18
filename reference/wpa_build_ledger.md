# Build the WPA net ledger

Each team's players sum to its result minus `start` (win = 1, draw =
0.5, loss = 0): +0.5, 0 or -0.5 with the default even start. The two
teams in a match cancel.

## Usage

``` r
wpa_build_ledger(
  actions,
  wp_actions,
  fixtures,
  lineups,
  adj = NULL,
  start = 0.5,
  dacts_share = 0,
  own_goal_credit = 0.25,
  own_goal_relief = 0.25,
  tol = 1e-06,
  verbose = TRUE
)
```

## Arguments

- actions:

  SPADL actions as the net goals ledger takes them (`match_id`,
  `action_id`, `team_id`, `player_id`, `action_type`, `result`,
  `period_id`, `time_seconds`, and optionally `xpass`,
  `receiver_player_id`, `receiver_team_id`). Any `epv_delta` or `xgot`
  column is ignored.

- wp_actions:

  One row per action: `match_id`, `action_id`, `team_id`, `wp` (the
  acting team's win chance) and `wpa` (its change, in the acting team's
  frame, NOT centred: `add_wp_vars(..., centre = FALSE)`).

- fixtures:

  `match_id`, `home_team_id`, `away_team_id`, `home_score`,
  `away_score`.

- lineups:

  Lineups as
  [`ng_spread_pools()`](https://peteowen1.github.io/panna/reference/ng_spread_pools.md)
  and
  [`ng_player_game()`](https://peteowen1.github.io/panna/reference/ng_player_game.md)
  take them.

- adj:

  Optional output of
  [`ng_build_adjacency()`](https://peteowen1.github.io/panna/reference/ng_build_adjacency.md).

- start:

  The even start every team is measured from. Default 0.5.

- dacts_share:

  Passed to
  [`ng_spread_pools()`](https://peteowen1.github.io/panna/reference/ng_spread_pools.md).
  Default 0 (flat pools).

- own_goal_credit:

  Share of the scoring side's gain from an own goal paid to its last
  player on the ball before it, as his own (`wpa_own`, role
  `own_goal_setup`); the rest stays in the team pool, spread by minutes.
  The player who put it in his own net keeps his blame apart from
  `own_goal_relief`. Default 0.25 (Pete, 2026-11-19: a bit more credit
  for the player before, not all of it). 0 gives the old split.

- own_goal_relief:

  Share of the own-goal scorer's blame moved to his team's pool, spread
  by minutes (Pete, 2026-11-19: a bit less blame, not a full removal).
  Default 0.25; 0 gives the old split.

- tol:

  Largest allowed gap between a team's total and its target, and between
  the engine's match total and (result - opening win chance).

- verbose:

  Print a summary. Default `TRUE`.

## Value

One row per player-match: `wpa_neutral` (the ledger value),
`wpa_offence`, `wpa_defence`, `wpa_start`
(`wpa_offence + wpa_defence + wpa_start = wpa_neutral`), torp's three
channels `wpa_own` (on-ball acts), `wpa_won` (balls won and shots
stopped, where the feed names him) and `wpa_shared` (his minutes' share
of the team pools plus `wpa_start`), with
`wpa_own + wpa_won + wpa_shared = wpa_neutral`, and one `wpa_<role>`
column per engine role. Attribute `targets`: per team-match, its result,
target, opening win chance and the gap after the build.

## Details

The engine is
[`ng_build_ledger()`](https://peteowen1.github.io/panna/reference/ng_build_ledger.md)
in its team convention, with the goals-only rules switched off:

- `shot_chain` and `shot_aftermath` restart value from 0 goals after a
  shot, which has no meaning in win probability;

- the xGOT split prices a shot's strike as `xgot - xG` **in goals**
  (`R/epv_net_goals.R`, `strike <- xgt - xg0`), so on a win probability
  row it would hand the shooter and the keeper amounts in the wrong
  units. Shots take the single-step rule, as torp's plain configuration
  does.

The engine conserves each match's own total, which for un-centred WPA is
the result minus the opening win chance (the model's first action). The
difference between that opening and `start` – about +/-0.04 per team,
the home advantage in the model – is spread across each team's players
by minutes and reported as `wpa_start`.

## See also

Other win probability:
[`add_wp_match_elo()`](https://peteowen1.github.io/panna/reference/add_wp_match_elo.md),
[`add_wp_vars()`](https://peteowen1.github.io/panna/reference/add_wp_vars.md),
[`add_wp_xi_strength()`](https://peteowen1.github.io/panna/reference/add_wp_xi_strength.md),
[`aggregate_player_game_wpa()`](https://peteowen1.github.io/panna/reference/aggregate_player_game_wpa.md),
[`assign_wpa_credit()`](https://peteowen1.github.io/panna/reference/assign_wpa_credit.md),
[`create_wp_features()`](https://peteowen1.github.io/panna/reference/create_wp_features.md),
[`load_wp_model()`](https://peteowen1.github.io/panna/reference/load_wp_model.md),
[`predict_wp()`](https://peteowen1.github.io/panna/reference/predict_wp.md),
[`save_wp_model()`](https://peteowen1.github.io/panna/reference/save_wp_model.md),
[`train_wp_model()`](https://peteowen1.github.io/panna/reference/train_wp_model.md),
[`train_wp_structural()`](https://peteowen1.github.io/panna/reference/train_wp_structural.md)
