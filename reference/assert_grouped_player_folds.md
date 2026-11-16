# Assert that no player straddles more than one CV fold

The R5 checklist item made executable: fails loudly if any `player_id`
appears with more than one distinct `foldid` value. Intended to be
called on every `foldid` before it reaches
[`glmnet::cv.glmnet()`](https://rdrr.io/pkg/glmnet/man/cv.glmnet.html) –
[`fit_spm_panel()`](https://peteowen1.github.io/panna/reference/fit_spm_panel.md)
calls this on its own grouped assignment as a self-check, and it is
exported so any other candidate/eval script constructing folds by hand
can reuse it as a tripwire.

## Usage

``` r
assert_grouped_player_folds(foldid, player_id)
```

## Arguments

- foldid:

  Integer vector of fold assignments.

- player_id:

  Character vector, same length as `foldid`.

## Value

Invisibly `TRUE` if grouped; aborts otherwise.

## See also

Other spm panel:
[`assert_asof_panel_window()`](https://peteowen1.github.io/panna/reference/assert_asof_panel_window.md),
[`build_spm_panel()`](https://peteowen1.github.io/panna/reference/build_spm_panel.md),
[`classify_role_group()`](https://peteowen1.github.io/panna/reference/classify_role_group.md),
[`fit_spm_panel()`](https://peteowen1.github.io/panna/reference/fit_spm_panel.md),
[`fit_spm_panel_xgb()`](https://peteowen1.github.io/panna/reference/fit_spm_panel_xgb.md),
[`make_grouped_player_foldid()`](https://peteowen1.github.io/panna/reference/make_grouped_player_foldid.md),
[`predict_spm_panel()`](https://peteowen1.github.io/panna/reference/predict_spm_panel.md),
[`predict_spm_panel_net()`](https://peteowen1.github.io/panna/reference/predict_spm_panel_net.md),
[`predict_spm_panel_xgb()`](https://peteowen1.github.io/panna/reference/predict_spm_panel_xgb.md)
