# Create splints for all matches

Generates splint data for an entire dataset. Uses data.table for fast
pre-splitting by match_id.

## Usage

``` r
create_all_splints(
  processed_data,
  include_goals = TRUE,
  verbose = TRUE,
  chunk_by = c("league", "none")
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

## Value

List with combined splint data
