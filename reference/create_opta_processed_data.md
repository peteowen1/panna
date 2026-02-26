# Create Processed Data Structure from Opta Data

Creates the processed_data list structure expected by
create_all_splints() from Opta data sources. This is the main entry
point for using Opta data with the splint creation pipeline.

## Usage

``` r
create_opta_processed_data(
  opta_lineups,
  opta_events = NULL,
  opta_shot_events = NULL,
  opta_stats = NULL,
  use_goals_as_xg = FALSE
)
```

## Arguments

- opta_lineups:

  Data frame from load_opta_lineups()

- opta_events:

  Data frame from load_opta_events()

- opta_shot_events:

  Data frame from load_opta_shot_events()

- opta_stats:

  Optional data frame from load_opta_stats() (for match metadata)

- use_goals_as_xg:

  Logical. Use goals as xG proxy if TRUE. Default FALSE.

## Value

List with components: lineups, events, shooting, results, stats_summary

## Examples

``` r
if (FALSE) { # \dontrun{
# Load Opta data
lineups <- load_opta_lineups("ENG", "2024-2025")
events <- load_opta_events("ENG", "2024-2025")
shots <- load_opta_shot_events("ENG", "2024-2025")

# Create processed data structure
processed <- create_opta_processed_data(lineups, events, shots)

# Create splints
splints <- create_all_splints(processed)
} # }
```
