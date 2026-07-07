# Get a player's value metrics profile

Returns a summary of all value metrics for a player: EPR (from per-game
EPV), PSR (from estimated skills), per-game EPV/WPA/PSV averages, and
the combined panna value. Loads from cached pipeline output.

## Usage

``` r
player_value(player = NULL, season = NULL, source = c("local", "remote"))
```

## Arguments

- player:

  Character string – player name (partial match, case-insensitive).
  E.g., `"Salah"`, `"H. Kane"`.

- season:

  Season filter (e.g., `"2024-2025"`). If NULL, uses the most recent
  available season.

- source:

  Data source: `"local"` (default, pipeline caches) or `"remote"`
  (GitHub Releases).

## Value

A list with:

- player_name:

  Matched player name

- summary:

  data.table of season totals and per-90 rates across EPV / WPA / PSV
  metric families

- game_log_epv:

  Per-game EPV data.table (or NULL if unavailable)

- game_log_wpa:

  Per-game WPA data.table (or NULL)

- game_log_psv:

  Per-game PSV data.table (or NULL)

- epr:

  EPR result (or NULL)
