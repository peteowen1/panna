# Aggregate player minutes and split into regular/replacement pools

Aggregate player minutes and split into regular/replacement pools

## Usage

``` r
.aggregate_player_minutes(players, splints, min_minutes)
```

## Arguments

- players:

  Data frame of player appearances per splint

- splints:

  Data frame of splints with duration

- min_minutes:

  Minimum total minutes for inclusion as regular player

## Value

List with player_minutes, replacement_player_ids, player_ids, n_players,
all_player_minutes
