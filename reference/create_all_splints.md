# Create splints for all matches

Generates splint data for an entire dataset. Uses data.table for fast
pre-splitting by match_id.

## Usage

``` r
create_all_splints(processed_data, include_goals = TRUE, verbose = TRUE)
```

## Arguments

- processed_data:

  List of processed data from process_all_data

- include_goals:

  Whether to create splints at goal times

- verbose:

  Print progress messages

## Value

List with combined splint data
