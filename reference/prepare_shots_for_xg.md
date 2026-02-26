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

- is_penalty: Binary indicator for penalties

- is_direct_freekick: Binary for direct free kicks

- shot_type\_\*: One-hot encoded shot types

- is_goal: Target variable (1 = goal)

## Examples

``` r
if (FALSE) { # \dontrun{
shots <- load_opta_shot_events("ENG", "2024-2025")
shot_features <- prepare_shots_for_xg(shots)
} # }
```
