# Merge Duplicate Duel Rows

Opta records both participants of a duel as separate rows (winner and
loser perspectives). This function merges these into single rows,
keeping the winner's row and adding opponent information from the
loser's row. Fully vectorized with data.table for speed.

## Usage

``` r
merge_duel_rows(dt)
```

## Arguments

- dt:

  Data.table in SPADL format with duel actions (modified in place)

## Value

Data.table with merged duel rows and new columns:

- opponent_player_id - Player who lost the duel (NA for non-duels)

- opponent_player_name - Name of opponent
