# Calculate Distance to Goal

Calculates Euclidean distance from coordinates to center of goal.
Assumes goal is at x=100, y=50.

## Usage

``` r
calculate_distance_to_goal(x, y, goal_x = 100, goal_y = 50)
```

## Arguments

- x:

  X coordinate (0-100 scale)

- y:

  Y coordinate (0-100 scale)

- goal_x:

  X coordinate of goal center (default 100)

- goal_y:

  Y coordinate of goal center (default 50)

## Value

Distance to goal in coordinate units

## Examples

``` r
if (FALSE) { # \dontrun{
calculate_distance_to_goal(50, 50)  # Halfway line
} # }
```
