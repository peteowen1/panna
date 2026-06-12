# Build training dataset for the minutes-projection model

For each (player_id x match) in the international competitions, computes
backward-looking features as of the match date and pairs them with the
realized `minutes_played` (the supervised target).

## Usage

``` r
build_minutes_training_data(
  lineups,
  intl_comps,
  ratings_path = "data-raw/cache-opta/07_seasonal_ratings.rds",
  min_team_matches = 5L,
  verbose = TRUE
)
```

## Arguments

- lineups:

  Data.table of opta lineups (full, not just intl).

- intl_comps:

  Character vector of competition codes treated as "international".
  Training rows are restricted to these.

- ratings_path:

  Path to seasonal xRAPM RDS.

- min_team_matches:

  Drop teams with fewer than this many intl matches.

- verbose:

  Logical.

## Value

A data.table with features + `minutes_played` target. The vector of
training feature column names is in `attr(result, "feature_cols")`.
