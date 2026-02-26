# Convert Opta Match Events to SPADL Format

Transforms Opta event data into a standardized SPADL-like format
suitable for EPV modeling. Coordinates are preserved in Opta's native
team-relative format where each team's actions are from their own
perspective (x=0 is own goal, x=100 is opponent's goal). This is the
correct format for EPV models where each action is evaluated from the
ball-carrier's perspective.

## Usage

``` r
convert_opta_to_spadl(opta_events, normalize_direction = FALSE)
```

## Arguments

- opta_events:

  Data frame from load_opta_match_events()

- normalize_direction:

  Whether to attempt coordinate normalization. Set to FALSE (default) to
  preserve Opta's team-relative coordinates, which is correct for EPV.
  Set to TRUE only for visualizations requiring unified pitch
  coordinates (note: the normalization heuristic may not work well with
  team-relative data).

## Value

Data frame in SPADL format with columns:

- match_id: Match identifier

- action_id: Sequential action number within match

- period_id: 1=first half, 2=second half

- time_seconds: Time in seconds from period start

- team_id: Team performing action

- player_id: Player performing action

- player_name: Player name

- start_x, start_y: Starting coordinates (0-100)

- end_x, end_y: Ending coordinates (0-100)

- action_type: Standardized action type

- action_type_id: Numeric action type code

- opta_type_id: Original Opta type_id (e.g., 13=saved, 15=missed,
  16=goal)

- result: "success" or "fail"

- bodypart: "foot", "head", or "other"

- receiver_player_id: Player who receives/intercepts (from next action)

- receiver_player_name: Name of receiver/interceptor

- receiver_team_id: Team of receiver (for detecting possession change)

- possession_change: TRUE if next action is by opponent team

## Examples

``` r
if (FALSE) { # \dontrun{
events <- load_opta_match_events("ENG", "2024-2025")
spadl <- convert_opta_to_spadl(events)
} # }
```
