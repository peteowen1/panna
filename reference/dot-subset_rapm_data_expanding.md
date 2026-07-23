# Row-subset a prepared pooled RAPM design to seasons strictly before a cutoff year (optionally also at-or-after a minimum year), dropping resulting all-zero player columns

Mirrors FABLE-ASOF-EXPERIMENTS.md sec 5.2 Step A, generalized from
"exclude season S" (LOSO) to "keep seasons \< cutoff_year" (expanding
window). BOX-SCORE-VALUE-SPM-REDESIGN.md sec 2.1 further generalizes
this to a bounded window (`min_year <= season_end_year < cutoff_year`)
for the windowed prior-free RAPM target – `min_year = NULL` (default)
preserves the original expanding-window behaviour unchanged. Season-only
players and season-only league-season dummy columns become all-zero once
their rows are dropped; both are removed here (kept off/def-symmetric
per player) so the resulting design has no dead columns.

## Usage

``` r
.subset_rapm_data_expanding(
  rapm_data,
  splint_season_map,
  cutoff_year,
  min_year = NULL
)
```

## Arguments

- rapm_data:

  The `rapm_data` list as produced by
  [`prepare_rapm_data()`](https://peteowen1.github.io/panna/reference/prepare_rapm_data.md)
  / saved in `04_rapm.rds$rapm_data` (needs `X_full`, `y`, `weights`,
  `player_ids`, `covariate_names`, `player_mapping`,
  `row_data$splint_id`).

- splint_season_map:

  data.frame/data.table with `splint_id`, `season_end_year` (e.g.
  `03_splints.rds$splints[, c("splint_id", "season_end_year")]`).

- cutoff_year:

  Integer; rows from seasons `< cutoff_year` are kept.

- min_year:

  Integer or `NULL` (default). When supplied, rows from seasons
  `< min_year` are additionally dropped, bounding the window to
  `min_year <= season_end_year < cutoff_year`. `NULL` keeps the original
  "seasons \< cutoff_year" (expanding-window) behaviour.

## Value

A `rapm_data`-shaped list (row- and column-subset), suitable for
[`fit_rapm()`](https://peteowen1.github.io/panna/reference/fit_rapm.md).
