# Fit Bradley-Terry-Davidson team ratings

Given match-level W/D/L probabilities (from the prediction pipeline),
fit a single strength rating per team. Uses the Davidson (1970)
extension that folds draws into Bradley-Terry:
`P(draw) prop nu.exp((r_i+r_j)/2)`.

## Usage

``` r
fit_bt_ratings(predictions, neutral = FALSE, max_iter = 200L, verbose = TRUE)
```

## Arguments

- predictions:

  Data frame with columns `home_team`, `away_team`, `prob_H`, `prob_D`,
  `prob_A`.

- neutral:

  Logical or vector of same length as `nrow(predictions)`. If TRUE, the
  home-field advantage parameter is set to zero for that row. Default
  FALSE (every row treated as a home/away pair).

- max_iter:

  Integer. L-BFGS-B max iterations. Default 200.

- verbose:

  Logical. Print fit diagnostics. Default TRUE.

## Value

A list with:

- `ratings`: data frame of team / rating / rank

- `home_adv`: scalar home-advantage parameter (log-odds)

- `nu`: draw-frequency parameter

- `loss`: final cross-entropy loss

- `converged`: optim convergence flag

## Details

Optimisation: minimise cross-entropy between predicted probs and
BT-implied probs via L-BFGS-B. Ratings are centered to mean zero for
interpretability.

## See also

Other world cup simulation:
[`bt_match_prob()`](https://peteowen1.github.io/panna/reference/bt_match_prob.md),
[`build_knockout_lookup()`](https://peteowen1.github.io/panna/reference/build_knockout_lookup.md),
[`compute_league_offsets()`](https://peteowen1.github.io/panna/reference/compute_league_offsets.md),
[`match_is_international()`](https://peteowen1.github.io/panna/reference/match_is_international.md),
[`mirror_match_rows()`](https://peteowen1.github.io/panna/reference/mirror_match_rows.md),
[`run_wc2026_reference_checks()`](https://peteowen1.github.io/panna/reference/run_wc2026_reference_checks.md),
[`simulate_world_cup()`](https://peteowen1.github.io/panna/reference/simulate_world_cup.md)
