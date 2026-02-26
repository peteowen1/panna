# Create unique player ID

Generates a unique identifier for a player based on name and team. Note:
This is a simple implementation; FBref player URLs are more reliable.

## Usage

``` r
create_player_id(player_name, fbref_id = NULL)
```

## Arguments

- player_name:

  Player name

- fbref_id:

  Optional FBref player ID (preferred if available

## Value

Character vector of player IDs
