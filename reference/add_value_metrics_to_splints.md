# Add value metric columns to splints

Joins per-game player value metrics (EPV, WPA, PSV) to splint data,
aggregating to team-level totals within each splint. Values are prorated
by splint duration relative to total match minutes.

## Usage

``` r
add_value_metrics_to_splints(
  splint_data,
  player_game_epv = NULL,
  player_game_wpa = NULL,
  player_game_psv = NULL
)
```

## Arguments

- splint_data:

  List with `splints` and `players` data.frames (from
  [`create_all_splints()`](https://peteowen1.github.io/panna/reference/create_all_splints.md)).

- player_game_epv:

  Per-game EPV from
  [`aggregate_player_game_epv()`](https://peteowen1.github.io/panna/reference/aggregate_player_game_epv.md).
  If NULL, EPV columns are not added.

- player_game_wpa:

  Per-game WPA from
  [`aggregate_player_game_wpa()`](https://peteowen1.github.io/panna/reference/aggregate_player_game_wpa.md).
  If NULL, WPA columns are not added.

- player_game_psv:

  Per-game PSV from
  [`calculate_psv()`](https://peteowen1.github.io/panna/reference/calculate_psv.md).
  If NULL, PSV columns are not added.

## Value

The `splint_data` list with additional columns on the `splints`
data.frame: `epv_home/epv_away`, `wpa_home/wpa_away`,
`psv_home/psv_away`.

## Details

This allows RAPM to be trained on EPV, WPA, or PSV as response variables
alongside the default xG target.
