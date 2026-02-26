# Prepare Pass Data for xPass Model

Creates features for pass completion probability modeling from SPADL
actions. Optimized with data.table for fast processing.

## Usage

``` r
prepare_passes_for_xpass(spadl_actions)
```

## Arguments

- spadl_actions:

  SPADL actions data frame with pass events

## Value

Data frame with pass features:

- start_x, start_y: Pass origin coordinates

- end_x, end_y: Pass destination coordinates

- pass_distance: Euclidean distance

- pass_angle: Angle of pass relative to goal direction

- is_forward: Pass moves toward opponent goal

- is_progressive: Significant forward movement

- start_zone, end_zone: Pitch zones

- crosses_midfield: Pass crosses halfway line

- into_penalty_area: Pass ends in penalty area

- into_final_third: Pass ends in final third

- pass_type\_\*: Pass type indicators

- completed: Target variable (1 = successful pass)

## Examples

``` r
if (FALSE) { # \dontrun{
spadl <- convert_opta_to_spadl(events)
pass_features <- prepare_passes_for_xpass(spadl)
} # }
```
