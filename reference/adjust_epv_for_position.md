# Adjust Player EPV Credits for Position

Subtracts the positional mean from each EPV component, so players are
compared to peers at the same position rather than across all positions.
Uses Opta lineup positions (Goalkeeper, Defender, Defensive Midfielder,
Midfielder, Attacking Midfielder, Striker). Wing Back merged into
Defender.

## Usage

``` r
adjust_epv_for_position(player_match, credit_cols = "total_credit")
```

## Arguments

- player_match:

  data.table with columns: player_id, match_id, position, and one or
  more credit columns to center

- credit_cols:

  Character vector of column names to position-center. Defaults to
  "total_credit".

## Value

Same data.table with `{col}_adj` columns added for each credit col
