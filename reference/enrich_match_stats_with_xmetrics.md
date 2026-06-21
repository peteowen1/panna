# Enrich per-match stats with per-match xMetrics (xG + finishing/keeper value)

Left-joins per-player-match xG and the redesign's over-performance
features (npg/ibox/obox `g_minus_xg`, `placement_added`, keeper `gsaa`,
plus `xg_per90`/`npxg_per90`/xA/xPass) onto a box-score `match_stats`
table by `(player_id, match_id)`, sourcing the per-match
`xmetrics_bymatch/` artifact via
[`load_opta_xmetrics`](https://peteowen1.github.io/panna/reference/load_opta_xmetrics.md)`(by_match = TRUE)`.
Shared by the skills estimation (step 2) and the PSR/PSV coefficient
training (step 7) so both see the identical feature set (avoids the
train/serve drift of a duplicated inline join).

## Usage

``` r
enrich_match_stats_with_xmetrics(
  match_stats,
  verbose = TRUE,
  fail_if_missing_frac = Inf
)
```

## Arguments

- match_stats:

  data.table/data.frame with `league`, `season`, `match_id`,
  `player_id`.

- verbose:

  Print join diagnostics. Default `TRUE`.

- fail_if_missing_frac:

  Numeric in `[0, 1]`. If the fraction of league-seasons whose
  `xmetrics_bymatch/` file fails to load exceeds this,
  [`stop()`](https://rdrr.io/r/base/stop.html) instead of silently
  training on a partly-xG-blind dataset. Default `Inf` (library-safe:
  never fails). Pipeline callers that require the features (skills step
  2, PSR step 7) should pass a finite value (e.g. `0.5`). A total miss
  (no files at all) always errors when this is finite, regardless of the
  fraction.

## Value

`match_stats` (as data.table) with the xMetrics columns added (NA-filled
to 0 for player-matches with no shots). Returns input unchanged (with a
warning) if key columns are missing or no bymatch files are found and
`fail_if_missing_frac` is `Inf`.
