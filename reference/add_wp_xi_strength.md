# Join starting-XI strength onto WP features

Adds `xi_diff` = home XI strength minus away XI strength. A team's XI
strength is 11 x the mean rating of its rated starters, each starter's
rating taken from the latest as-of snapshot dated BEFORE match day
(`snapshot <= match_date - 1`, at most `roll_days` old). Use a leak-free
as-of rating such as `career_panna_asof.parquet` (each monthly snapshot
fitted only on matches up to its date); the weekly PSR is NOT leak-free
for model evaluation, because its coefficients are fitted on all seasons
(panna#273, 2026-11-10). A team with fewer than `min_rated` rated
starters gets `NA`, never 0.

## Usage

``` r
add_wp_xi_strength(
  wp_features,
  lineups,
  ratings,
  roll_days = 62L,
  min_rated = 9L
)
```

## Arguments

- wp_features:

  Output of
  [`create_wp_features`](https://peteowen1.github.io/panna/reference/create_wp_features.md)
  (needs `match_id`, `team_id`, `is_home`).

- lineups:

  Lineups with `match_id`, `team_id`, `player_id`, `match_date`,
  `is_starter`.

- ratings:

  As-of ratings: `player_id`, `ref_date` (or `snapshot_date`) and
  `panna` (or `rating`).

- roll_days:

  Oldest snapshot accepted, in days (default 62: monthly snapshots).

- min_rated:

  Fewest rated starters for a team value (default 9).

## Value

`wp_features` as a data.table with `xi_diff`; attribute `xi_table` holds
the per team-match values.

## See also

Other win probability:
[`add_wp_match_elo()`](https://peteowen1.github.io/panna/reference/add_wp_match_elo.md),
[`add_wp_vars()`](https://peteowen1.github.io/panna/reference/add_wp_vars.md),
[`aggregate_player_game_wpa()`](https://peteowen1.github.io/panna/reference/aggregate_player_game_wpa.md),
[`assign_wpa_credit()`](https://peteowen1.github.io/panna/reference/assign_wpa_credit.md),
[`create_wp_features()`](https://peteowen1.github.io/panna/reference/create_wp_features.md),
[`load_wp_model()`](https://peteowen1.github.io/panna/reference/load_wp_model.md),
[`predict_wp()`](https://peteowen1.github.io/panna/reference/predict_wp.md),
[`save_wp_model()`](https://peteowen1.github.io/panna/reference/save_wp_model.md),
[`train_wp_model()`](https://peteowen1.github.io/panna/reference/train_wp_model.md),
[`train_wp_structural()`](https://peteowen1.github.io/panna/reference/train_wp_structural.md),
[`wpa_build_ledger()`](https://peteowen1.github.io/panna/reference/wpa_build_ledger.md)
