# Row-subset a prepared pooled RAPM design to seasons strictly before a cutoff year, dropping resulting all-zero player columns

Mirrors FABLE-ASOF-EXPERIMENTS.md sec 5.2 Step A, generalized from
"exclude season S" (LOSO) to "keep seasons \< cutoff_year" (expanding
window). Season-only players and season-only league-season dummy columns
become all-zero once their rows are dropped; both are removed here (kept
off/def-symmetric per player) so the resulting design has no dead
columns.

## Usage

``` r
.subset_rapm_data_expanding(rapm_data, splint_season_map, cutoff_year)
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

## Value

A `rapm_data`-shaped list (row- and column-subset), suitable for
[`fit_rapm()`](https://peteowen1.github.io/panna/reference/fit_rapm.md).
