# Cached position multipliers, warning on any gap

Returns `decay_params$position_multipliers` (or computes all of them
from `dt` when none are cached). Stats missing from the cache are NOT
filled from `dt`, because callers pass slices and a per-slice multiplier
would differ between training and serving; they get a warning instead.
Skills step 02b caches every detected stat (review R015: the 2026-09
cache covered 52 of 88, so a back-up keeper's saves shrank toward the
all-player mean 0.15 per 90 rather than the keeper mean 1.76).

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

Named list of position multipliers.
