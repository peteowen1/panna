# Simulate a 48-team World Cup

Simulate a 48-team World Cup

## Usage

``` r
simulate_world_cup(
  predictions,
  groups,
  knockout,
  n_sims = 10000L,
  elo_k = 20,
  bracket = c("fifa2026", "random"),
  verbose = TRUE
)
```

## Arguments

- predictions:

  Data frame with `home_team`, `away_team`, `prob_H`, `prob_D`,
  `prob_A`, `pred_home_goals`, `pred_away_goals` for all group-stage
  fixtures.

- groups:

  Data frame mapping `team` -\> `group` (12 groups of 4). Group letters
  must match the official draw when `bracket = "fifa2026"` – the letters
  determine knockout paths.

- knockout:

  Output of
  [`build_knockout_lookup()`](https://peteowen1.github.io/panna/reference/build_knockout_lookup.md)
  – a list with `probs` (pairwise knockout probabilities) and `team_elo`
  (named vector of pre-tournament Elo, used as the run-hot baseline).

- n_sims:

  Integer. Default 10000.

- elo_k:

  Run-hot Elo K-factor (default 20, matching the production pipeline in
  11_simulate_wc2026.R; 0 disables momentum).

- bracket:

  `"fifa2026"` (default) plays the knockouts on the official 2026
  bracket (matches 73-104) with FIFA's third-place slot eligibility;
  `"random"` reshuffles the round of 32 each sim (the pre-2026-06-11
  behaviour). Falls back to `"random"` with a warning if `groups` does
  not contain exactly groups A-L of 4 teams each.

- verbose:

  Logical. Print progress. Default TRUE.

## Value

A list with `summary` (per-team round probabilities), `group_table`
(group-position probabilities), `n_sims`, `elo_k`, and `bracket` (the
bracket actually used – `"random"` if the fifa2026 fallback fired).
