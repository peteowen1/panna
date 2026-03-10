# Resolve decay lambda for a stat

Looks up per-stat override, then falls back to category default.

## Usage

``` r
.resolve_lambda(stat_name, decay_params, eff_map = NULL)
```

## Arguments

- stat_name:

  Name of the stat

- decay_params:

  Decay parameter list

- eff_map:

  Named list from
  [`.classify_skill_stats()`](https://peteowen1.github.io/panna/reference/dot-classify_skill_stats.md)

## Value

Numeric lambda value
