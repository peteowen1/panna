# Apply canonical-id mapping to a data.table that has a player_id column

Replaces `player_id` with the canonical mapping in-place. Useful before
joining lineups to ratings, where the same person should map to a single
xRAPM row regardless of which alt-id their match used.

## Usage

``` r
apply_canonical_player_ids(dt, canon)
```

## Arguments

- dt:

  Data.table with a player_id column.

- canon:

  Output of
  [`build_player_id_canonical_map()`](https://peteowen1.github.io/panna/reference/build_player_id_canonical_map.md).

## Value

The input dt with `player_id` rewritten to canonical.
