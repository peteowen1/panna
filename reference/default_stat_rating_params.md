# Default stat rating hyperparameters

Returns default decay rates and prior strengths for the soccer stat
rating estimation pipeline. Can be customized per-stat via named
overrides.

## Usage

``` r
default_stat_rating_params()
```

## Value

A list with elements:

- rate:

  Decay lambda for rate (per-90) stats, default 0.003 (~231 day
  half-life)

- efficiency:

  Decay lambda for efficiency stats, default 0.002 (~347 day half-life)

- xmetrics:

  Decay lambda for xMetrics stats, default 0.003

- prior_90s:

  Gamma prior strength in equivalent 90-minute matches, default 2

- prior_attempts:

  Beta prior strength in equivalent attempts, default 50
