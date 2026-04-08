# Get Player Positions from Opta Lineups

Extracts each player's most common starting position across a season.
Wing Back is merged into Defender. Substitutes without a starting
position are assigned one based on their average pitch x-coordinate.

## Usage

``` r
get_player_positions(lineups, spadl_actions = NULL)
```

## Arguments

- lineups:

  data.table from load_opta_lineups()

- spadl_actions:

  Optional SPADL actions for fallback position assignment based on
  average x-coordinate

## Value

data.table with player_id and position columns
