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
