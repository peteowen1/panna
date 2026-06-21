# Estimate pre-match skills at multiple dates (incremental batch version)

Highly optimized for sequential date estimation. Instead of
re-processing all historical data at each date, maintains running
cumulative sums that are decayed forward and incrementally updated with
new observations. Uses [`rowsum()`](https://rdrr.io/r/base/rowsum.html)
(C-level) for grouped matrix sums.

## Usage

``` r
.detect_skill_stat_cols(dt)
```

## Arguments

- dt:

  data.frame/data.table of match stats.

- match_stats:

  Match-level stats (output of `compute_match_level_opta_stats`).

- ref_dates:

  Character or Date vector of dates to estimate skills at.

- decay_params:

  Decay parameters (default:
  [`get_default_decay_params()`](https://peteowen1.github.io/panna/reference/get_default_decay_params.md)).

- min_weighted_90s:

  Minimum weighted 90s for inclusion (default 3).

- verbose:

  Print progress (default TRUE).

## Value

Named list of data.tables (one per ref_date), keyed by date string. Each
table has one row per player with skill columns.

Canonical stat-column detector — ONE source of truth

Returns the modelled stat columns present in `dt`: per-90 rates (`_p90`
AND `_per90` — the xMetrics over-performance rates), the
efficiency/ratio stats from
[`.classify_skill_stats()`](https://peteowen1.github.io/panna/reference/dot-classify_skill_stats.md),
and the registered PSR/GK skill-col lists (catches `_xmetrics`-suffixed
cols).

Why this exists: the same `grep("_p90$")` detection was duplicated
across `.estimate_prematch_skills_batch` (psr.R),
`estimate_player_skills`, `compute_position_multipliers`, and
`adjust_match_stats_for_context` (estimated_skills.R). The `_p90$`-only
pattern silently dropped EVERY `_per90` xMetrics column — and fixing one
copy left the others broken (the train/serve skew we hit twice). All
detectors now route through here so a new feature can't be dropped by
one divergent copy.

Character vector of stat column names present in `dt`.

## Details

Complexity: O(N + D \* new_rows_per_date) instead of O(N \* D). For
typical data (~1M rows, 659 dates), this is ~100-300x faster.
