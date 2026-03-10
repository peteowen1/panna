# Resolve prior strength for a stat

Looks up per-stat override, then falls back to rate/efficiency default.

## Usage

``` r
.resolve_prior_strength(stat_name, decay_params, is_efficiency = FALSE)
```

## Arguments

- stat_name:

  Name of the stat

- decay_params:

  Decay parameter list

- is_efficiency:

  Logical — is this an efficiency stat?

## Value

Numeric prior strength
