# Calculate Physical Discontinuity

Calculates spatial discontinuity between consecutive actions using a
unified pitch coordinate frame. This converts team-relative Opta
coordinates to a common physical frame, allowing measurement of actual
ball movement rather than perspective changes.

## Usage

``` r
calculate_physical_discontinuity(spadl_dt)
```

## Arguments

- spadl_dt:

  Data.table or data.frame in SPADL format from
  [`convert_opta_to_spadl()`](https://peteowen1.github.io/panna/reference/convert_opta_to_spadl.md).
  Must have columns: match_id, team_id, start_x, start_y, end_x, end_y.

## Value

Data.table with additional columns:

- phys_disc: Physical discontinuity (Euclidean distance between action
  end and next action start in unified frame)

- phys_end_x, phys_end_y: End coordinates in physical frame

- phys_next_x, phys_next_y: Next action start in physical frame

## Details

Opta uses team-relative coordinates where each team sees x=100 as their
attacking goal. When possession changes, coordinates appear to "jump"
due to perspective change, not ball movement. This function removes that
artifact by converting all coordinates to a single reference frame.

For validation purposes, physical discontinuity should be low for most
action types (median \< 10-15 units). High values indicate data quality
issues or unusual game situations (e.g., set pieces).

Expected median physical discontinuity by action type:

- pass: ~3 (pass lands where receiver starts)

- aerial: ~3 (winner catches at their position)

- tackle: ~6 (tackler wins ball nearby)

- shot: ~10 (shot to goal, next action at keeper/goal)

## Examples

``` r
if (FALSE) { # \dontrun{
events <- load_opta_match_events("ENG", "2024-2025")
spadl <- convert_opta_to_spadl(events)
spadl_with_disc <- calculate_physical_discontinuity(spadl)

# Check median discontinuity by action type
spadl_with_disc[, .(median_disc = median(phys_disc, na.rm = TRUE)),
                by = action_type]
} # }
```
