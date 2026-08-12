# Validate an n_prev argument (internal helper)

Guards the sequence-feature lookback. The dangerous value is 0: the
loops below used `1:n_prev`, and `1:0` is `c(1, 0)`, so asking for "no
sequence features" instead produced a `_prev0` column set where
`shift(x, 0)` is the identity – i.e. the CURRENT action's outcome
leaking in as a "previous action" feature. Target leakage, silently.

## Usage

``` r
.check_n_prev(n_prev)
```
