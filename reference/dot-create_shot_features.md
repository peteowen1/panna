# Create shot features from coordinates and context

Shared helper for building xG prediction features. Used by both
[`prepare_shots_for_xg()`](https://peteowen1.github.io/panna/reference/prepare_shots_for_xg.md)
(training) and
[`add_xg_to_spadl()`](https://peteowen1.github.io/panna/reference/add_xg_to_spadl.md)
(inference).

## Usage

``` r
.create_shot_features(
  x,
  y,
  bodypart = NULL,
  situation = NULL,
  is_big_chance = 0L
)
```

## Arguments

- x, y:

  Numeric vectors of shot coordinates

- bodypart:

  Character vector of body part (e.g., "head", "foot_right", "right
  foot")

- situation:

  Character vector of shot situation (e.g., "open play", "set piece")

- is_big_chance:

  Integer vector (0/1) for big chances

## Value

Data frame of xG features (location, body part, situation)
