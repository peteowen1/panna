# Narrow a match_stats table to the columns skill estimation reads

One-call wrapper around
[`.detect_skill_stat_cols()`](https://peteowen1.github.io/panna/reference/dot-detect_skill_stat_cols.md) +
[`.compute_snapshot_loop_columns()`](https://peteowen1.github.io/panna/reference/dot-compute_snapshot_loop_columns.md) +
the `[[`-extraction construction (a bracket-select would copy the wide
table; see the data.table narrowing gotchas). Exists so every pipeline
site that loads `01_match_stats.rds` (predictions steps 02/02b) narrows
through ONE shared code path — the 02/02b inline copies had already
diverged once (02b narrowed since panna#133, 02 didn't), which kept step
02 at the full 421-column ~5.9GB footprint.

## Usage

``` r
.narrow_match_stats_for_skills(
  match_stats,
  extra_cols = c("player_id", "player_name", "match_date", "position", "total_minutes")
)
```

## Arguments

- match_stats:

  The full-width match_stats data.frame/data.table.

- extra_cols:

  Identity/context columns to always keep. Default covers the
  predictions-pipeline consumers
  ([`estimate_player_skills()`](https://peteowen1.github.io/panna/reference/estimate_player_skills.md)
  and its position/denominator internals).

## Value

A narrowed data.table (column references shared with the input, not
copied — the dropped columns free at the next
[`gc()`](https://rdrr.io/r/base/gc.html)).

## Details

Two registry-orphan columns are kept defensively:
`keeper_throws_accuracy` (listed in
[`.get_psr_efficiency_cols()`](https://peteowen1.github.io/panna/reference/dot-get_psr_efficiency_cols.md)
but missing from the skill registries — a registry desync, not dead
data) and `poss_won_att_ratio` (computed into the cache by
[`.calculate_opta_derived_features()`](https://peteowen1.github.io/panna/reference/dot-calculate_opta_derived_features.md)
but matched by no detector). Dropping them belongs to a deliberate
registry cleanup, not a narrowing pass.
