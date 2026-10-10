# Price a shot at more than its xG: the value it leaves behind

A shot that does not score still leaves its side something: a corner, a
rebound, the ball. On ENG 2024-2025 the state straight after a non-goal
shot is worth +0.034 goals to the shooting side on average (8,690
shots), against a mean xG of 0.112. Pricing the shot at its xG alone
under-pays the pass before it and the decision to shoot, and hands that
value to whoever touches the ball next.

## Usage

``` r
.ng_shot_aftermath(dt, fit = NULL, verbose = TRUE)
```

## Arguments

- dt:

  The ledger's actions (modified by reference), with `epv`, `epv_delta`,
  `result`, `team_id`, `action_type`.

- fit:

  Optional: a fit returned by an earlier call (`intercept`, `slope`),
  used instead of fitting on `dt`.

- verbose:

  Print the fit.

## Value

`list(dt, fit)`. With fewer than 50 non-goal shots and no `fit` given,
the line is `NG_SHOT_AFTERMATH_LINE` and `fit$fallback` is `TRUE`; this
is said as a message at the time, not a deferred warning.

## Details

Here a shot is worth `V0 = xG + (1 - xG) * A`, where `A` is a straight
line in xG fitted on the season's own non-goal shots: the value of the
next state to the shooting side, or 0 when nothing follows in the
period. It is refitted on every call, as torp reprices stoppages per
season. The row before a shot targeted its xG, so it moves up by
`V0 - xG`; a shot that does not score ends at the next row's start value
in its own frame; a goal still ends at 1. Own goals are left alone.
`shot_xg` keeps each repriced shot's xG, which the credit rules need to
split the row.
