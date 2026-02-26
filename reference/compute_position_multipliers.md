# Compute position-specific multipliers for prior centers

For each stat, computes the ratio of position-specific weighted average
to the overall weighted average: `multiplier = pos_avg / global_avg`.
This allows the prior center to be position-specific while keeping the
global optimization.

## Usage

``` r
compute_position_multipliers(match_stats, stat_cols = NULL)
```

## Arguments

- match_stats:

  A data.table with match-level stats and a `position` column.

- stat_cols:

  Character vector of stat columns. If NULL, auto-detects.

## Value

A named list where each element is a named numeric vector of length 4
(GK, DEF, MID, FWD) giving the multiplier for that stat and position.
