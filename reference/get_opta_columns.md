# Get Opta Column Names

Returns column names available in Opta data without loading the full
dataset.

## Usage

``` r
get_opta_columns(
  table_type = c("player_stats", "shots", "shot_events", "match_events", "events",
    "lineups")
)
```

## Arguments

- table_type:

  One of "player_stats", "shots", "shot_events", "events", or "lineups".

## Value

Character vector of column names.

## Examples

``` r
if (FALSE) { # \dontrun{
# See all player stats columns
get_opta_columns("player_stats")

# See shot event columns (individual shots with x/y)
get_opta_columns("shot_events")

# See match event columns (ALL events with x/y)
get_opta_columns("match_events")

# See event columns (goals, cards, subs)
get_opta_columns("events")

# See lineup columns
get_opta_columns("lineups")
} # }
```
