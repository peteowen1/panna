# Soccer stat rating definitions

Returns a data.frame describing all stats used in the skill estimation
and PSR/PSV pipelines. Each stat has a type (rate or efficiency),
category (offensive, defensive, goalkeeper, xmetrics), and metadata
about adjustment.

## Usage

``` r
soccer_stat_rating_definitions()
```

## Value

A data.frame with columns:

- stat_name:

  Column name in match stats

- type:

  "rate" (Gamma-Poisson) or "efficiency" (Beta-Binomial)

- category:

  "offensive", "defensive", "goalkeeper", "xmetrics", or "general"

- pos_adjusted:

  Logical; TRUE if prior is position-specific
