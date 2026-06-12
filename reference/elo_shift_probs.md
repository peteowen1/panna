# Nudge match probabilities by a run-hot Elo drift (internal helper)

Shifts the win/loss split by `drift` Elo points (logit shift) while
leaving the draw probability unchanged. `drift = 0` returns the input
untouched.

## Usage

``` r
elo_shift_probs(p1, pd, p2, drift)
```
