# Join pre-match team strength onto WP features

Adds `elo_diff` (home Elo minus away Elo, BEFORE the match) to the rows
of `wp_features` by `match_id`, for
[`train_wp_structural`](https://peteowen1.github.io/panna/reference/train_wp_structural.md)
and the structural branch of
[`predict_wp`](https://peteowen1.github.io/panna/reference/predict_wp.md).
Rows whose match has no Elo are reported and, by default, dropped –
never filled with 0, which would silently read as "evenly matched".

## Usage

``` r
add_wp_match_elo(
  wp_features,
  match_elo,
  min_coverage = 0.95,
  drop_missing = TRUE
)
```

## Arguments

- wp_features:

  Output of
  [`create_wp_features`](https://peteowen1.github.io/panna/reference/create_wp_features.md).

- match_elo:

  Data frame with `match_id` and `elo_diff`, or with `home_elo` and
  `away_elo`; e.g. the `per_match` element of
  [`compute_match_elos`](https://peteowen1.github.io/panna/reference/compute_match_elos.md),
  whose values are pre-match.

- min_coverage:

  Abort when fewer than this share of matches get an Elo (default 0.95).

- drop_missing:

  Drop rows with no Elo (default `TRUE`).

## Value

`wp_features` as a data.table with an `elo_diff` column.

## See also

Other win probability:
[`add_wp_vars()`](https://peteowen1.github.io/panna/reference/add_wp_vars.md),
[`add_wp_xi_strength()`](https://peteowen1.github.io/panna/reference/add_wp_xi_strength.md),
[`aggregate_player_game_wpa()`](https://peteowen1.github.io/panna/reference/aggregate_player_game_wpa.md),
[`assign_wpa_credit()`](https://peteowen1.github.io/panna/reference/assign_wpa_credit.md),
[`create_wp_features()`](https://peteowen1.github.io/panna/reference/create_wp_features.md),
[`load_wp_model()`](https://peteowen1.github.io/panna/reference/load_wp_model.md),
[`predict_wp()`](https://peteowen1.github.io/panna/reference/predict_wp.md),
[`save_wp_model()`](https://peteowen1.github.io/panna/reference/save_wp_model.md),
[`train_wp_model()`](https://peteowen1.github.io/panna/reference/train_wp_model.md),
[`train_wp_structural()`](https://peteowen1.github.io/panna/reference/train_wp_structural.md),
[`wpa_build_ledger()`](https://peteowen1.github.io/panna/reference/wpa_build_ledger.md)
