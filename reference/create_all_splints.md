# Create splints for all matches

Generates splint data for an entire dataset. Uses data.table for fast
pre-splitting by match_id.

## Usage

``` r
create_all_splints(
  processed_data,
  include_goals = TRUE,
  verbose = TRUE,
  chunk_by = c("league", "none"),
  min_splint_duration = 5
)
```

## Arguments

- processed_data:

  List of processed data from process_all_data

- include_goals:

  Whether to create splints at goal times

- verbose:

  Print progress messages

- chunk_by:

  Chunking strategy for memory efficiency. `"league"` (default)
  processes matches grouped by league to reduce peak memory usage.
  `"none"` processes all matches at once (original behaviour).

- min_splint_duration:

  Minimum splint duration in minutes (default 5). Soft boundaries
  (subs/goals/red cards) within this window of the most recently kept
  boundary are merged. Hard boundaries (kickoff, halftime, full-time)
  are always kept. Set to 0 to disable merging entirely.

## Value

List with combined splint data
