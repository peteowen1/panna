# Score panel rows against a fitted offense/defense pair, combining to a net prediction

[`fit_spm_panel()`](https://peteowen1.github.io/panna/reference/fit_spm_panel.md)
is fit separately per target (offense/defense have different sign
constraints and, for a real RAPM O/D split, different underlying
signal).

## Usage

``` r
predict_spm_panel_net(fits, newdata, lambda = c("min", "1se"))
```

## Arguments

- fits:

  List with `offense` and `defense` elements, each a
  [`fit_spm_panel()`](https://peteowen1.github.io/panna/reference/fit_spm_panel.md)
  result (as produced by the candidate configs in
  `data-raw/spm-redesign/05c_candidates.R`).

- newdata:

  Panel-shaped data.table/data.frame.

- lambda:

  `"min"` (default) or `"1se"`.

## Value

data.table(player_id, vintage_year (if present), pred_offense,
pred_defense, pred_net).

## Details

Sign convention (Pete, 2026-09-03): `defense_target` is built directly
from the published `defense` column (R/spm_panel.R's panel-building
step, `defense_target = defense`), which as of the same date is POSITIVE
= GOOD
([`extract_rapm_ratings()`](https://peteowen1.github.io/panna/reference/extract_rapm_ratings.md)/[`extract_xrapm_ratings()`](https://peteowen1.github.io/panna/reference/extract_xrapm_ratings.md),
R/rapm_model.R, now negate `def_coefs` at extraction). So `pred_defense`
is already positive=good, and the net prediction is
`pred_offense + pred_defense`.

**Do not "fix" this back to a minus sign.** An earlier version of this
function used `-` when `defense_target` was still negative=good (that
was correct THEN: net RAPM = offense - defense in the old convention),
and summing the two at that time flipped the defense half's contribution
at eval time and tanked every candidate's next-window correlation –
caught in the 2026-07-22 full-panel bake-off. The `+` here is correct
ONLY because the underlying target's sign flipped with it; the two
changes must always travel together.

## See also

Other spm panel:
[`assert_asof_panel_window()`](https://peteowen1.github.io/panna/reference/assert_asof_panel_window.md),
[`assert_grouped_player_folds()`](https://peteowen1.github.io/panna/reference/assert_grouped_player_folds.md),
[`build_spm_panel()`](https://peteowen1.github.io/panna/reference/build_spm_panel.md),
[`classify_role_group()`](https://peteowen1.github.io/panna/reference/classify_role_group.md),
[`fit_spm_panel()`](https://peteowen1.github.io/panna/reference/fit_spm_panel.md),
[`fit_spm_panel_xgb()`](https://peteowen1.github.io/panna/reference/fit_spm_panel_xgb.md),
[`make_grouped_player_foldid()`](https://peteowen1.github.io/panna/reference/make_grouped_player_foldid.md),
[`predict_spm_panel()`](https://peteowen1.github.io/panna/reference/predict_spm_panel.md),
[`predict_spm_panel_xgb()`](https://peteowen1.github.io/panna/reference/predict_spm_panel_xgb.md)
