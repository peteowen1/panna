# Build the SPM training panel: one row per (player, vintage year)

BOX-SCORE-VALUE-SPM-REDESIGN.md sec 2.2/3.1: for each vintage `Y` in
`vintage_years`, features are the player's box+xMetrics per-90
aggregates over `[Y - window_years, Y)`
([`.spm_panel_window_features()`](https://peteowen1.github.io/panna/reference/dot-spm_panel_window_features.md)),
target is the SAME-window prior-free RAPM for `Y`
(`rapm_window_targets.rds`, 04b_rapm_window_targets.R). Feature/target
window alignment is checked, not assumed (`strict_window_check`).

## Usage

``` r
build_spm_panel(
  match_stats,
  rapm_window_targets,
  vintage_years = 2019:2026,
  window_years = 5L,
  leagues = NULL,
  include_xmetrics = TRUE,
  xmetrics_source = c("local", "remote"),
  include_gk = FALSE,
  strict_window_check = TRUE
)
```

## Arguments

- match_stats:

  data.table,
  [`compute_match_level_opta_stats()`](https://peteowen1.github.io/panna/reference/compute_match_level_opta_stats.md)
  shape (e.g. `cache-skills/01_match_stats.rds`). Must have
  `season_end_year` OR `season` (the latter is converted via
  [`extract_season_end_year()`](https://peteowen1.github.io/panna/reference/extract_season_end_year.md)).

- rapm_window_targets:

  The list from `rapm_window_targets.rds` (04b_rapm_window_targets.R),
  keyed by vintage year.

- vintage_years:

  Integer vector of vintage years to build (default `2019:2026`,
  matching Wave 1).

- window_years:

  Window length (default 5, matching sec 2.1). Must match the window
  `rapm_window_targets` was built with, or `strict_window_check` aborts.

- leagues:

  Optional character vector to restrict to (`NULL` = all – full build;
  pass a small set for a smoke-scale build).

- include_xmetrics:

  Attempt best-effort xMetrics enrichment per vintage (default `TRUE`).
  Degrades gracefully (box-only, `cli_warn`) if local/remote xMetrics
  coverage is unavailable for a vintage's window – see
  [`.spm_panel_window_features()`](https://peteowen1.github.io/panna/reference/dot-spm_panel_window_features.md).

- xmetrics_source:

  `"local"` or `"remote"`.

- include_gk:

  Include GK rows in the returned panel (default `FALSE` – see above).
  When `FALSE`, dropped-row counts are reported via
  [`cli::cli_inform()`](https://cli.r-lib.org/reference/cli_abort.html).

- strict_window_check:

  Abort if a vintage's actual window (min_year, cutoff_year) doesn't
  match the corresponding `rapm_window_targets` entry's `window`
  (default `TRUE` – window misalignment defeats the whole point of the
  panel design; sec 2.2).

## Value

data.table, one row per (player_id, vintage_year), with feature columns,
`role`, `role_group`, `window_minutes`, `n_matches`, `offense_target`,
`defense_target`, `rapm_target` (net), `vintage_year`,
`window_min_year`, `window_max_year`. Attributes: `target_provenance`
(`"prior_free_rapm_window"`), `builder_params` (list of the arguments
above + `built_at`).

## Details

Circularity guard: calls `assert_prior_free_target(rapm_window_targets)`
before touching any rating, and stamps the returned panel's
`target_provenance` attribute so
[`fit_spm_panel()`](https://peteowen1.github.io/panna/reference/fit_spm_panel.md)
can re-verify it without needing the raw target object again.

GK rows are excluded by default (`include_gk = FALSE`) – panna#159's
keeper-rows-only design is the intended home for GK `spm_value`/SPM
pricing (BOX-SCORE-VALUE-SPM-REDESIGN.md sec 3.1, R7); this panel is
outfield-only until that lands.

## See also

Other spm panel:
[`assert_asof_panel_window()`](https://peteowen1.github.io/panna/reference/assert_asof_panel_window.md),
[`assert_grouped_player_folds()`](https://peteowen1.github.io/panna/reference/assert_grouped_player_folds.md),
[`classify_role_group()`](https://peteowen1.github.io/panna/reference/classify_role_group.md),
[`fit_spm_panel()`](https://peteowen1.github.io/panna/reference/fit_spm_panel.md),
[`fit_spm_panel_xgb()`](https://peteowen1.github.io/panna/reference/fit_spm_panel_xgb.md),
[`make_grouped_player_foldid()`](https://peteowen1.github.io/panna/reference/make_grouped_player_foldid.md),
[`predict_spm_panel()`](https://peteowen1.github.io/panna/reference/predict_spm_panel.md),
[`predict_spm_panel_net()`](https://peteowen1.github.io/panna/reference/predict_spm_panel_net.md),
[`predict_spm_panel_xgb()`](https://peteowen1.github.io/panna/reference/predict_spm_panel_xgb.md)
