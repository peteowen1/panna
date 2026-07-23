# Build the panel design matrix: global feature columns + optional role-group x feature deviation columns

Deviation column naming: `dev__<role_group>__<feature>` (role_group
codes from
[`classify_role_group()`](https://peteowen1.github.io/panna/reference/classify_role_group.md)
are alphanumeric-only, so this is a safe, unambiguous split point –
[`.panel_base_feature_of()`](https://peteowen1.github.io/panna/reference/dot-panel_base_feature_of.md)
reverses it).

## Usage

``` r
.build_panel_design_matrix(
  panel,
  global_cols,
  deviation_cols = character(0),
  role_groups = character(0)
)
```

## Arguments

- panel:

  data.table/data.frame with `global_cols` and (if `deviation_cols`
  non-empty) a `role_group` column.

- global_cols:

  Character vector of predictor column names.

- deviation_cols:

  Character vector (subset of `global_cols`) to generate role-group
  deviation columns for. `character(0)` = no pooling.

- role_groups:

  Character vector of role-group codes to build deviations for
  (typically
  [`.spm_panel_outfield_role_groups()`](https://peteowen1.github.io/panna/reference/dot-spm_panel_outfield_role_groups.md)).

## Value

List: `X` (numeric matrix, columns = `global_cols` then all `dev__*`
columns in group-major order), `dev_names` (character vector, possibly
empty).
