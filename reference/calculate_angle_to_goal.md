# Calculate Angle to Goal

Calculates the visible angle (in radians) to the goal from given
coordinates. The angle represents how much of the goal is visible from
the player's position. Useful for shot xG models.

## Usage

``` r
calculate_angle_to_goal(x, y, goal_width = 12)
```

## Arguments

- x:

  X coordinate (0-100 scale, attacking toward x=100)

- y:

  Y coordinate (0-100 scale, y=50 is center of pitch)

- goal_width:

  Goal width in coordinate units (default ~12 for standard goal)

## Value

Angle to goal in radians (always positive)

## Examples

``` r
if (FALSE) { # \dontrun{
calculate_angle_to_goal(90, 80)  # Wide position - smaller angle
calculate_angle_to_goal(50, 50)  # Halfway line - small angle
} # }
```
