# Create splints for a single match (fast version)

Optimized version that takes pre-filtered data for a single match.
Derives goal times from shooting data if events are not available.
Derives red card times from stats_summary if available.

## Usage

``` r
create_match_splints_fast(
  match_id,
  events,
  lineups,
  shooting,
  results,
  stats = NULL,
  include_goals = TRUE
)
```

## Arguments

- match_id:

  Match identifier

- events:

  Pre-filtered events for this match

- lineups:

  Pre-filtered lineups for this match

- shooting:

  Pre-filtered shooting for this match

- results:

  Pre-filtered results for this match

- stats:

  Pre-filtered stats_summary for this match (for red cards)

- include_goals:

  Whether to create splints at goal times

## Value

List with splint data for the match
