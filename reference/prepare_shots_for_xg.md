# Prepare Shot Data for xG Model

Prepares Opta shot event data with features needed for xG modeling.

## Usage

``` r
prepare_shots_for_xg(shot_events)
```

## Arguments

- shot_events:

  Data frame from load_opta_shot_events()

## Value

Data frame with xG features:

- x, y: Shot coordinates (normalized)

- distance_to_goal: Distance from shot to goal center

- angle_to_goal: Visible angle to goal

- is_header: Binary indicator for headed shots

- is_big_chance: Binary indicator for big chances

- is_penalty: Binary flag used to EXCLUDE penalties from training
  (`exclude_penalties = TRUE`); they are scored at
  [`PENALTY_XG`](https://peteowen1.github.io/panna/reference/PENALTY_XG.md)
  instead, since every penalty is taken from the same spot and there is
  nothing for the geometry features to learn

- is_open_play, is_set_piece, is_corner: situation flags. NOTE the
  source labels for open play and set piece appear transposed – see
  [`.create_shot_features()`](https://peteowen1.github.io/panna/reference/dot-create_shot_features.md)
  for the evidence and why they are left as-is rather than silently
  inverted

- is_goal: Target variable (1 = goal)

## Examples

``` r
if (FALSE) { # \dontrun{
shots <- load_opta_shot_events("ENG", "2024-2025")
shot_features <- prepare_shots_for_xg(shots)
} # }
```
