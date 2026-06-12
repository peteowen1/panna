# Prepare a cached lineups data object for fast minutes-feature queries

Runs the global / per-team / per-player passes once and caches the
intermediate data structures needed by
[`query_minutes_features()`](https://peteowen1.github.io/panna/reference/query_minutes_features.md).
Re-run only when the underlying lineups parquet changes.

## Usage

``` r
prepare_minutes_cache(
  lineups,
  intl_comps,
  ratings_path = "data-raw/cache-opta/07_seasonal_ratings.rds",
  cache_path = NULL,
  verbose = TRUE
)
```

## Arguments

- lineups:

  Data.frame/data.table of opta lineups (full, both intl and club).

- intl_comps:

  Character vector of competition codes treated as "international".

- ratings_path:

  Path to seasonal xRAPM RDS (for panna lookups).

- cache_path:

  Optional RDS file path. If supplied, saves the result.

- verbose:

  Logical.

## Value

A list with:

- `per_player_history` – keyed data.table per (player_id, date_int) with
  `mins_intl, mins_club, app_intl, start_intl, start_club, is_intl`

- `cumsum_lookup` – per-player precomputed cumulative-sum arrays for
  fast rolling-window queries

- `modal_role` – modal non-Substitute role per player

- `player_panna` – panna ratings table (player_id x season_end_year)

- `team_rotation` – global rotation_idx per team

- `team_intl_count` – number of intl matches per team
