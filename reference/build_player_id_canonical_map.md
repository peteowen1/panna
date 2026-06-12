# Build a player_id -\> canonical_id lookup

Build a player_id -\> canonical_id lookup

## Usage

``` r
build_player_id_canonical_map(
  lineups,
  min_dominance_ratio = 0.05,
  require_team_overlap = TRUE
)
```

## Arguments

- lineups:

  Data.table of lineups (full or filtered).

- min_dominance_ratio:

  Numeric. Alt must have at most this fraction of the main's appearance
  count to be a merge candidate. Default 0.05 (alt \<= 5% of main).
  Catches data-entry errors (alt has 2-10 matches vs main has hundreds)
  without merging mid-tier namesakes.

- require_team_overlap:

  Logical. Require at least one common team between alt and main.
  Default TRUE. Catches the "two Danilos played for different national
  teams" namesake case.

## Value

Data.table with columns `player_id` (every observed id) and
`canonical_id` (its main mapping; equals `player_id` for non-merged
dominant ids).
