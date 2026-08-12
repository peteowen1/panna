# Play one knockout round (internal helper)

Resolves all ties in a round from the integer-indexed knockout matrices,
applies the run-hot Elo nudge, draws a scoreline conditional on the
drawn outcome, and updates both teams' dynamic Elo. Round-level
randomness is drawn in blocks. A 90-minute draw goes to a penalty
shootout but counts as a draw for the Elo update.

## Usage

``` r
play_knockout_round(bracket, WIN, DRAW, SC, elo_dyn, elo_base, elo_k)
```

## Arguments

- SC:

  Scoreline tables from
  [`build_scoreline_tables()`](https://peteowen1.github.io/panna/reference/build_scoreline_tables.md),
  with one column per ordered pair at `i + (j - 1) * n_teams`.
