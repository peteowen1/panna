# Create splints for a single match

Master function to create all splint data for one match.

## Usage

``` r
create_match_splints(
  match_id,
  events,
  lineups,
  shooting,
  results,
  include_goals = TRUE
)
```

## Arguments

- match_id:

  Match identifier

- events:

  Processed events data

- lineups:

  Processed lineups data

- shooting:

  Processed shooting data

- results:

  Processed match results

- include_goals:

  Whether to create splints at goal times

## Value

List with splint data for the match
