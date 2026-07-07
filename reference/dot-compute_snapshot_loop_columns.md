# Compute the column set a per-date skill snapshot loop actually needs

Extracted from `08b_export_psr_weekly.R` (panna#128) so the
column-narrowing logic that fixed the weekly-snapshot OOM is
unit-testable rather than living inline. `match_stats` caches carry 400+
box-score/ metadata columns;
[`estimate_player_skills()`](https://peteowen1.github.io/panna/reference/estimate_player_skills.md)
only reads `stat_cols` plus identity columns, but ALSO looks up raw
denominator columns for efficiency-ratio stats (e.g. `shots` for
`shot_accuracy`) via `.compute_denominator()` — those aren't themselves
in `stat_cols` and are easy to silently drop when narrowing by hand
(this exact risk is why the fix ships with this test-covered helper
instead of inline column-list construction).

## Usage

``` r
.compute_snapshot_loop_columns(
  available_cols,
  stat_cols,
  extra_cols = character(0)
)
```

## Arguments

- available_cols:

  Character vector — the full set of columns present in the source
  match_stats table (typically `names(match_stats)`).

- stat_cols:

  Character vector of stat columns the caller intends to estimate (e.g.
  `stat_cols_all` in 08b).

- extra_cols:

  Character vector of additional identity/context columns to always keep
  (e.g. `player_id`, `match_date`). `NA`/ `NULL` entries are dropped so
  callers can pass an optional column (like a possibly-absent league
  column) unconditionally.

## Value

Character vector of column names to keep, intersected with
`available_cols` — safe to pass straight to a `[[`-based narrowing
construction.
