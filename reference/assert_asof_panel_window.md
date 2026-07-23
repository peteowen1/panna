# Assert that an as-of training panel for eval vintage `Y` contains no rows from a LATER vintage

The structural leak guard for the held-out next-window eval gate (sec
5.2): a candidate being scored on vintage `Y`'s rows against the `Y+1`
target must be fit ONLY on `vintage_year <= Y` rows. Fitting on the
WHOLE panel (all vintages pooled) and then scoring vintage `Y` against
`Y+1`'s target is the exact hindsight leak `eval_nextseason.R`'s header
documents as banned for its own pooled-vs-per-season candidate
distinction: the pooled fit would have TRAINED on vintage `Y+1`'s own
panel row (whose label literally IS the `Y+1` target being scored
against), and vintage `Y`'s window overlaps vintage `Y+1`'s window in 4
of 5 seasons, so the pooled model's coefficients partially encode
information that shouldn't be visible yet.
`data-raw/spm-redesign/05c_candidates.R`'s `run_candidate_asof()` calls
this on its own `vintage_year <= Y` subset before fitting – this is the
assertion made executable, not just a comment.

## Usage

``` r
assert_asof_panel_window(train_panel, Y)
```

## Arguments

- train_panel:

  A panel (or panel subset) about to be passed to
  [`fit_spm_panel()`](https://peteowen1.github.io/panna/reference/fit_spm_panel.md)/`run_candidate()`
  for an as-of fit targeting eval vintage `Y`.

- Y:

  The eval vintage the fit is being restricted to (features/target for
  `Y` must come from `vintage_year <= Y` only).

## Value

Invisibly `TRUE` if no row exceeds `Y`; aborts otherwise.

## See also

Other spm panel:
[`assert_grouped_player_folds()`](https://peteowen1.github.io/panna/reference/assert_grouped_player_folds.md),
[`build_spm_panel()`](https://peteowen1.github.io/panna/reference/build_spm_panel.md),
[`classify_role_group()`](https://peteowen1.github.io/panna/reference/classify_role_group.md),
[`fit_spm_panel()`](https://peteowen1.github.io/panna/reference/fit_spm_panel.md),
[`fit_spm_panel_xgb()`](https://peteowen1.github.io/panna/reference/fit_spm_panel_xgb.md),
[`make_grouped_player_foldid()`](https://peteowen1.github.io/panna/reference/make_grouped_player_foldid.md),
[`predict_spm_panel()`](https://peteowen1.github.io/panna/reference/predict_spm_panel.md),
[`predict_spm_panel_net()`](https://peteowen1.github.io/panna/reference/predict_spm_panel_net.md),
[`predict_spm_panel_xgb()`](https://peteowen1.github.io/panna/reference/predict_spm_panel_xgb.md)
