# Score panel rows against a fitted offense/defense pair, combining to a net prediction

[`fit_spm_panel()`](https://peteowen1.github.io/panna/reference/fit_spm_panel.md)
is fit separately per target (offense/defense have different sign
constraints and, for a real RAPM O/D split, different underlying
signal). The targets are stored in the RAW internal convention
(`defense_target` = contribution to opponent xG, positive = concedes
more = bad), and net RAPM = offense - defense
([`extract_rapm_ratings()`](https://peteowen1.github.io/panna/reference/extract_rapm_ratings.md),
R/rapm_model.R "RAPM rating = offense - defense") – so the net
prediction is `pred_offense - pred_defense`. (An earlier version summed
the two, which flipped the defense half's contribution at eval time and
tanked every candidate's next-window correlation – caught in the
2026-07-22 full-panel bake-off.)

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
