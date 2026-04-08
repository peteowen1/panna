# Build unified player game ratings

Merges per-game EPV, WPA, and PSV into a single data.table with one row
per player per match. Computes a combined `panna_value` blending EPV and
PSV contributions.

## Usage

``` r
build_player_game_ratings(
  player_game_epv,
  player_game_wpa = NULL,
  player_game_psv = NULL,
  epv_weight = PANNA_EPR_WEIGHT,
  psv_weight = PANNA_PSR_WEIGHT
)
```

## Arguments

- player_game_epv:

  Per-game EPV from
  [`aggregate_player_game_epv`](https://peteowen1.github.io/panna/reference/aggregate_player_game_epv.md).

- player_game_wpa:

  Per-game WPA from
  [`aggregate_player_game_wpa`](https://peteowen1.github.io/panna/reference/aggregate_player_game_wpa.md).
  Optional; WPA columns are NA if not provided.

- player_game_psv:

  Per-game PSV from
  [`calculate_psv_components`](https://peteowen1.github.io/panna/reference/calculate_psv_components.md).
  Optional; PSV columns are NA if not provided.

- epv_weight:

  Weight for EPV in combined panna_value (default `PANNA_EPR_WEIGHT`).

- psv_weight:

  Weight for PSV in combined panna_value (default `PANNA_PSR_WEIGHT`).

## Value

A data.table with one row per player per match:

- player_id, player_name, team_id, match_id:

  Identifiers

- minutes_played, position:

  From lineups (if available)

- epv_total, epv_offensive, epv_defensive:

  EPV components

- epv_p90:

  EPV per 90 minutes

- wpa_total, wpa_as_actor, wpa_as_receiver:

  WPA components

- wpa_p90:

  WPA per 90 minutes

- psv, osv, dsv:

  Player Stat Value with O/D decomposition

- panna_value:

  Combined: epv_weight \* epv_total + psv_weight \* psv

- panna_value_p90:

  Combined per 90 minutes
