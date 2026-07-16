# Canonical stat-column detector — ONE source of truth

Returns the modelled stat columns present in `dt`: per-90 rates (`_p90`
AND `_per90` — the xMetrics over-performance rates), the
efficiency/ratio stats from
[`.classify_skill_stats()`](https://peteowen1.github.io/panna/reference/dot-classify_skill_stats.md),
and the registered PSR/GK skill-col lists (catches `_xmetrics`-suffixed
cols).

## Usage

``` r
.detect_skill_stat_cols(dt)
```

## Arguments

- dt:

  data.frame/data.table of match stats.

## Value

Character vector of stat column names present in `dt`.

## Details

Why this exists: the same `grep("_p90$")` detection was duplicated
across `.estimate_prematch_skills_batch` (psr.R),
`estimate_player_skills`, `compute_position_multipliers`, and
`adjust_match_stats_for_context` (estimated_skills.R). The `_p90$`-only
pattern silently dropped EVERY `_per90` xMetrics column — and fixing one
copy left the others broken (the train/serve skew we hit twice). All
detectors now route through here so a new feature can't be dropped by
one divergent copy.
