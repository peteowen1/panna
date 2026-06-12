# Confederation Initial-Elo Priors

Per-confederation starting Elo. Without this, every team starts at 1500
and confederations only diverge via match results – but with few
cross-conf matches per year, that divergence is slow and stays biased
toward whichever confederation has the most internal-pool matches (= AFC
pool drifts up because they play each other a lot).

## Usage

``` r
ELO_CONFEDERATION_PRIORS
```

## Format

Named numeric vector mapping confederation -\> initial Elo.

## Details

Confederation priors give each pool a sensible starting position
informed by historical World Cup performance. v6 values come directly
from the DEoptim optimization (no parametric spread anymore – each delta
is tuned independently).
