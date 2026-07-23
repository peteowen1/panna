# Build one (player, vintage year) window of raw counting-stat totals

Sums the raw Opta counting columns
([`.get_opta_col_mapping()`](https://peteowen1.github.io/panna/reference/dot-get_opta_col_mapping.md)
panna names, already present on `match_stats` – it is
[`compute_match_level_opta_stats()`](https://peteowen1.github.io/panna/reference/compute_match_level_opta_stats.md)
output) over matches with `season_end_year` in
`[min_year, cutoff_year)`, derives a minutes-weighted modal
[`classify_role()`](https://peteowen1.github.io/panna/reference/classify_role.md)/[`classify_role_group()`](https://peteowen1.github.io/panna/reference/classify_role_group.md)
per player, then runs the SAME per-90 + derived-feature pipeline
[`aggregate_opta_stats()`](https://peteowen1.github.io/panna/reference/aggregate_opta_stats.md)
uses
([`.calculate_opta_per90()`](https://peteowen1.github.io/panna/reference/dot-calculate_opta_per90.md),
[`.calculate_opta_derived_features()`](https://peteowen1.github.io/panna/reference/dot-calculate_opta_derived_features.md))
so the window-level feature columns are byte-identical in
name/construction to the career-level SPM's – the column contract
[`.spm_opta_predictor_cols()`](https://peteowen1.github.io/panna/reference/dot-spm_opta_predictor_cols.md)
selects against.

## Usage

``` r
.spm_panel_window_features(
  match_stats,
  cutoff_year,
  min_year,
  leagues = NULL,
  include_xmetrics = TRUE,
  xmetrics_source = "local"
)
```

## Arguments

- match_stats:

  data.table,
  [`compute_match_level_opta_stats()`](https://peteowen1.github.io/panna/reference/compute_match_level_opta_stats.md)
  shape (needs `player_id`, `match_id`, `season_end_year`, `position`,
  `position_side`, `total_minutes`, `league`, plus the raw counting
  columns).

- cutoff_year:

  Integer; rows from seasons `< cutoff_year` are kept.

- min_year:

  Integer; rows from seasons `< min_year` are dropped (window is
  `[min_year, cutoff_year)`).

- leagues:

  Optional character vector to restrict to (`NULL` = all).

- include_xmetrics:

  Whether to attempt xMetrics enrichment (best-effort – see
  [`build_spm_panel()`](https://peteowen1.github.io/panna/reference/build_spm_panel.md)).

- xmetrics_source:

  `"local"` or `"remote"`, passed to
  [`enrich_match_stats_with_xmetrics()`](https://peteowen1.github.io/panna/reference/enrich_match_stats_with_xmetrics.md).

## Value

data.frame, one row per player, feature columns + `role`, `role_group`,
`window_minutes`, `n_matches`. `NULL` if no rows survive the
window/league filter. Attribute `xmetrics_included` (logical).
