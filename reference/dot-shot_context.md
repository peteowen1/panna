# Pre-shot context for every shot in an Opta event stream

For each shot (types 13-16, periods 1-4): its pre-shot tags, the assist
(the last pass by the shooting team tagged 210 in the 20 seconds
before), seconds since the other team last had the ball and completed
passes since, whether it follows another shot within 5 seconds, and the
score before it (own goals, qualifier 28, count for the other side).
Events are ordered by period, minute, second, then event_id.

## Usage

``` r
.shot_context(events)
```

## Arguments

- events:

  Opta events for whole matches: `match_id`, `event_id`, `type_id`,
  `team_id`, `period_id`, `minute`, `second`, `outcome`, `x`, `y`,
  `qualifier_json`. Pass every event of each match, not a filtered
  subset: possession and score are read from the events around the shot.

## Value

data.table, one row per shot: `match_id`, `event_id` and the context
columns (`q<tag>`, `has_assist`, `a<tag>`, `a_len`, `a_x`, `a_y`,
`poss_secs`, `poss_passes`, `rebound`, `goals_for`, `goals_against`,
`score_diff`, `minute`).
