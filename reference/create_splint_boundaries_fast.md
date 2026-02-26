# Create splint boundaries (fast version)

Create splint boundaries (fast version)

## Usage

``` r
create_splint_boundaries_fast(
  events,
  shots = NULL,
  include_goals = TRUE,
  include_halftime = TRUE,
  goal_times = NULL,
  goal_is_home = NULL,
  sub_times = NULL,
  red_card_times = NULL,
  red_card_is_home = NULL
)
```

## Arguments

- events:

  Event data (may be NULL)

- shots:

  Shots data with minute column (may be NULL)

- include_goals:

  Whether to include goal boundaries

- include_halftime:

  Whether to include halftime boundary

- goal_times:

  Numeric vector of goal minutes (derived from shooting)

- goal_is_home:

  Logical vector indicating if each goal was by home team

- sub_times:

  Numeric vector of substitution minutes (derived from lineups)

- red_card_times:

  Numeric vector of red card minutes (derived from stats)

- red_card_is_home:

  Logical vector indicating if each red card was for home team
