# Map a 16-role code (`classify_role()` output) to the plan's 6-group role taxonomy

BOX-SCORE-VALUE-SPM-REDESIGN.md sec 3.1: GK, CB, FB/WB {LB,RB,LWB,RWB},
DM/CM {DM,CM}, AM/Wide {CAM,LM,RM,LW,RW}, CF {CF,LF,RF}. Group codes are
alphanumeric-only (no `/`) so they're safe to use inside design-matrix
column names (`dev__<group>__<feature>`, see
[`.build_panel_design_matrix()`](https://peteowen1.github.io/panna/reference/dot-build_panel_design_matrix.md)).
`"UNK"` (blank/unrecognized
[`classify_role()`](https://peteowen1.github.io/panna/reference/classify_role.md)
output) and any other unmatched code map to `NA` – those rows get zero
role-deviation contribution (pure global-feature pricing) rather than
being forced into a group they don't belong to.

## Usage

``` r
classify_role_group(role)
```

## Arguments

- role:

  Character vector of
  [`classify_role()`](https://peteowen1.github.io/panna/reference/classify_role.md)
  16-role codes.

## Value

Character vector of 6-group codes (`"GK"`, `"CB"`, `"FBWB"`, `"DMCM"`,
`"AMWIDE"`, `"CF"`), or `NA` for unmatched input.

## See also

Other spm panel:
[`assert_asof_panel_window()`](https://peteowen1.github.io/panna/reference/assert_asof_panel_window.md),
[`assert_grouped_player_folds()`](https://peteowen1.github.io/panna/reference/assert_grouped_player_folds.md),
[`build_spm_panel()`](https://peteowen1.github.io/panna/reference/build_spm_panel.md),
[`fit_spm_panel()`](https://peteowen1.github.io/panna/reference/fit_spm_panel.md),
[`fit_spm_panel_xgb()`](https://peteowen1.github.io/panna/reference/fit_spm_panel_xgb.md),
[`make_grouped_player_foldid()`](https://peteowen1.github.io/panna/reference/make_grouped_player_foldid.md),
[`predict_spm_panel()`](https://peteowen1.github.io/panna/reference/predict_spm_panel.md),
[`predict_spm_panel_net()`](https://peteowen1.github.io/panna/reference/predict_spm_panel_net.md),
[`predict_spm_panel_xgb()`](https://peteowen1.github.io/panna/reference/predict_spm_panel_xgb.md)
