# Compute minutes-model features for a specific list of players at a date

Compute minutes-model features for a specific list of players at a date

## Usage

``` r
query_minutes_features(
  cache,
  player_ids,
  team_name,
  as_of_date,
  tournament_match_num = 1L,
  days_rest_team = NULL,
  is_tournament = 1L,
  is_friendly = 0L,
  tournament_start = NULL
)
```

## Arguments

- cache:

  Output of
  [`prepare_minutes_cache()`](https://peteowen1.github.io/panna/reference/prepare_minutes_cache.md).

- player_ids:

  Character vector of player_ids to predict for.

- team_name:

  The country these players are playing for.

- as_of_date:

  Date – the upcoming match date.

- tournament_match_num:

  Integer – which game in their tournament run.

- days_rest_team:

  Integer – days since this team's last intl match. If NULL, derived
  from cache.

- is_tournament:

  Integer 0/1 – group-stage / knockout (1) vs qualifier (0).

- is_friendly:

  Integer 0/1 – friendly (1) vs competitive (0). Default 0 for
  WC/qualifier predictions.

- tournament_start:

  Date. First day of the current tournament. When supplied,
  `tourn_mins_sofar` / `tourn_starts_sofar` accumulate the player's intl
  minutes/starts in `[tournament_start, as_of_date)`; otherwise they are
  0 (matches the training convention where non-tournament rows are
  zeroed).

## Value

Data.table with one row per player, columns matching the model's
`feature_cols`. Pass directly to
[`predict_minutes()`](https://peteowen1.github.io/panna/reference/predict_minutes.md).

## See also

Other expected minutes:
[`build_team_expected_minutes()`](https://peteowen1.github.io/panna/reference/build_team_expected_minutes.md),
[`classify_role()`](https://peteowen1.github.io/panna/reference/classify_role.md),
[`predict_minutes()`](https://peteowen1.github.io/panna/reference/predict_minutes.md),
[`prepare_minutes_cache()`](https://peteowen1.github.io/panna/reference/prepare_minutes_cache.md)
