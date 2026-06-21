# Compute Elo Ratings for All Matches

Iterates through matches chronologically and computes Elo ratings.
Returns BOTH per-match pre-match Elos (for joining onto the match
dataset) AND the final post-iteration team-Elo state (for looking up the
current Elo of teams in upcoming fixtures). Returning both is what
prevents step 03 from having to duplicate the iteration – the previous
duplicate-iteration approach was missing the same NA guards as this
function, which caused the 2026-05-28 NA-cascade bug where a single
NA-team friendly poisoned every team's Elo via NA-named-lookup.

## Usage

``` r
compute_match_elos(
  results,
  k = 20,
  home_advantage = 88,
  initial_elo = 1500,
  k_table = NULL,
  cross_conf_mult = 1,
  conf_priors = NULL,
  use_venue_factor = FALSE,
  time_decay_halflife = NULL,
  decay_reference_date = NULL,
  update_mode = c("outcome", "margin_sqrt"),
  blend_w = 0.5,
  margin_slope = 1.66
)
```

## Arguments

- results:

  Data frame with match_date, home_team, away_team, home_goals,
  away_goals columns, sorted by date

- k:

  K-factor (default 20)

- home_advantage:

  Home advantage in Elo points (default 65)

- initial_elo:

  Starting Elo (default 1500)

- k_table:

  Optional named numeric vector mapping league codes to per-match-type K
  values (e.g., `ELO_MATCH_TYPE_K`). When supplied,
  [`elo_match_k()`](https://peteowen1.github.io/panna/reference/elo_match_k.md)
  selects the K for each match by its league; otherwise every match uses
  the single `k` argument.

- cross_conf_mult:

  Numeric multiplier (default 1.0 = disabled) applied to K when home and
  away teams are in different confederations. Lets the model learn
  faster from cross-confederation matches that constrain the relative
  ordering between confederation prior centers.

- conf_priors:

  Optional named numeric vector of starting Elos per confederation
  (e.g., `c(UEFA=1500, CONMEBOL=1500, ...)`). When supplied, teams are
  initialized from their confederation's prior instead of the single
  `initial_elo`. Requires
  [`build_team_confederations()`](https://peteowen1.github.io/panna/reference/build_team_confederations.md)
  to be able to derive each team's confederation from `results`.

- use_venue_factor:

  Logical (default FALSE for backwards compat). When TRUE,
  `home_advantage` is scaled by
  [`compute_venue_factor()`](https://peteowen1.github.io/panna/reference/compute_venue_factor.md)
  per match – +1 for true home, 0 for neutral tournament, -1 when the
  designated "home_team" is actually visiting the host country.

- time_decay_halflife:

  Optional numeric (days, default NULL = disabled). When set, scales K
  by `0.5 ^ ((reference_date - match_date) / halflife)` so older matches
  contribute less to the Elo iteration. Useful for recency-weighting; v5
  optimization converged near "off" (~6500 days), so not default but
  available for callers wanting FIFA/SPI-style recency.

- decay_reference_date:

  Optional Date or date-coercible string used as "now" for the decay
  calculation. Defaults to `max(match_date)` in `results`.

- update_mode:

  Passed to
  [`update_elo()`](https://peteowen1.github.io/panna/reference/update_elo.md):
  "outcome" (default) or "margin_sqrt" (the xG-Elo form). When
  "margin_sqrt" and `results` carries `home_xg`/`away_xg` columns, the
  update targets a blended goals/xG margin (goal-diff fallback per row
  where xG is NA).

- blend_w, margin_slope:

  Passed to
  [`update_elo()`](https://peteowen1.github.io/panna/reference/update_elo.md)
  in margin_sqrt mode (weight on goals vs xG, and
  expected-margin-per-400-Elo).

## Value

A list with two elements:

- `per_match`: data frame with match_id, home_elo, away_elo, elo_diff
  (pre-match Elo for each match in the input order)

- `final_elos`: named numeric vector of post-iteration team Elos, for
  use with upcoming fixtures
