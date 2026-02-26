# Assign players to splints

Determines which players were on the pitch for each splint. Uses lineup
minutes and substitution events to track when players were actually on
the pitch.

## Usage

``` r
assign_players_to_splints(boundaries, lineups, events, match_id)
```

## Arguments

- boundaries:

  Data frame of splint boundaries

- lineups:

  Data frame of match lineups

- events:

  Data frame of match events (for substitutions)

- match_id:

  Match identifier

## Value

Data frame with player assignments per splint
