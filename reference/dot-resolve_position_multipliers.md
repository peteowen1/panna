# Position multipliers for every requested stat

Uses the cached `decay_params$position_multipliers` where present and
computes the rest from `dt`. Before this, a stat missing from the cached
list (36 of 88 skill stats in the 2026-09 cache: every keeper stat, xG,
GSAA, the duel WOE stats) got no positional prior centre, so a back-up
keeper's saves were shrunk toward the all-player mean (0.15 per 90)
rather than the keeper mean (1.76) (review R015).

## Usage

``` r
.resolve_position_multipliers(decay_params, dt, stat_cols)
```

## Arguments

- decay_params:

  Decay parameters, possibly holding position_multipliers.

- dt:

  Match-level stats with a `position` column.

- stat_cols:

  Stats that need a multiplier.

## Value

Named list of multipliers covering `stat_cols`.
