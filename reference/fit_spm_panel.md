# Fit a role-pooled elastic-net SPM model on the panel, predicting the windowed prior-free RAPM target

The Wave-2 estimator (BOX-SCORE-VALUE-SPM-REDESIGN.md sec 2.2/2.3/3.1):
plain
[`glmnet::cv.glmnet()`](https://rdrr.io/pkg/glmnet/man/cv.glmnet.html)
with (a) player-grouped CV folds (never random rows – R5), (b) an
optional role-group x feature interaction block for the restricted
role-ambivalent feature set, shrunk toward 0 via a higher
`penalty.factor` (partial pooling – no new machinery beyond glmnet), (c)
minutes weighting (`sqrt`/`linear`), (d) an optional errors-in-variables
target rescale, (e) sign constraints extending 05_spm.R's defense lists
with sec 3.1's new offense set.

## Usage

``` r
fit_spm_panel(
  panel,
  target = c("offense", "defense", "net"),
  role_pooling = TRUE,
  role_ambivalent_cols = .spm_panel_role_ambivalent_cols(),
  deviation_penalty_mult = 5,
  alpha = 0.5,
  weight_transform = c("sqrt", "linear"),
  min_window_minutes = 0,
  eiv_rescale = FALSE,
  eiv_m0 = 8000,
  eiv_floor = 0.4,
  sign_constraints = TRUE,
  predictor_cols = NULL,
  nfolds = 5,
  seed = NULL
)
```

## Arguments

- panel:

  Output of
  [`build_spm_panel()`](https://peteowen1.github.io/panna/reference/build_spm_panel.md)
  (or any data.table/data.frame carrying the same `target_provenance`
  attribute + columns).

- target:

  One of `"offense"`, `"defense"`, `"net"` – which target column
  (`offense_target`/`defense_target`/`rapm_target`) to fit.

- role_pooling:

  Add role-group deviation columns (default `TRUE`). `FALSE` =
  global-only (candidate S1).

- role_ambivalent_cols:

  Feature columns to build deviations for (default
  [`.spm_panel_role_ambivalent_cols()`](https://peteowen1.github.io/panna/reference/dot-spm_panel_role_ambivalent_cols.md)).

- deviation_penalty_mult:

  Multiplier applied to deviation columns' `penalty.factor` relative to
  global columns (default 5 – higher shrinkage, sec 3.1's
  partial-pooling design).

- alpha:

  Elastic-net mixing (default 0.5).

- weight_transform:

  `"sqrt"` (default) or `"linear"` (sec 2.3.1 – the plan restricts this
  study to these two;
  [`fit_spm_model()`](https://peteowen1.github.io/panna/reference/fit_spm_model.md)'s
  `"log"`/`"none"` are out of scope here).

- min_window_minutes:

  Drop panel rows below this window-minutes floor before fitting
  (default 0 = no floor; sec 2.3.1's blunt alternative to weighting).

- eiv_rescale:

  Apply the errors-in-variables target rescale
  `y / max(r_hat, eiv_floor)`,
  `r_hat = window_minutes / (window_minutes + eiv_m0)` (default `FALSE`;
  sec 2.3.2).

- eiv_m0:

  Implied prior minutes at 0 the ridge penalty is equivalent to (default
  8000 – the Wave-1 attenuation study's empirical estimate,
  `data-raw/spm-redesign/03_attenuation_diagnostics.R` /
  `attenuation_band_summary.csv`; only used when `eiv_rescale = TRUE`).

- eiv_floor:

  Minimum `r_hat` before rescaling (default 0.4, per sec 2.3.2).

- sign_constraints:

  Apply sec 3.1's sign constraints for the chosen `target` (default
  `TRUE`; no-op for `target = "net"`, which has no hand-curated list in
  05_spm.R either).

- predictor_cols:

  Global predictor columns (default `NULL` = the canonical
  `.spm_opta_predictor_cols(panel)` selector, guaranteeing feature-set
  parity with the existing career-level SPM).

- nfolds:

  CV folds (default 5).

- seed:

  RNG seed for the grouped fold assignment (default `NULL`).

## Value

A `cv.glmnet` object with `panna_metadata` (type `"spm_panel"`,
`target`, `predictor_cols` (global), `dev_names`, `role_groups`, config
echo, `feature_sds`, `n_observations`, `lambda_min`) –
[`predict_spm_panel()`](https://peteowen1.github.io/panna/reference/predict_spm_panel.md)
scores new panel rows against it.

## Details

Circularity guard: re-checks the panel's `target_provenance` attribute
via
[`assert_prior_free_target()`](https://peteowen1.github.io/panna/reference/assert_prior_free_target.md)
(the SAME function
[`build_spm_panel()`](https://peteowen1.github.io/panna/reference/build_spm_panel.md)
calls on the raw target) before fitting – a training entry point that
trusts an un-stamped panel is exactly the gap sec 2.4.1 closes.

## See also

Other spm panel:
[`assert_asof_panel_window()`](https://peteowen1.github.io/panna/reference/assert_asof_panel_window.md),
[`assert_grouped_player_folds()`](https://peteowen1.github.io/panna/reference/assert_grouped_player_folds.md),
[`build_spm_panel()`](https://peteowen1.github.io/panna/reference/build_spm_panel.md),
[`classify_role_group()`](https://peteowen1.github.io/panna/reference/classify_role_group.md),
[`fit_spm_panel_xgb()`](https://peteowen1.github.io/panna/reference/fit_spm_panel_xgb.md),
[`make_grouped_player_foldid()`](https://peteowen1.github.io/panna/reference/make_grouped_player_foldid.md),
[`predict_spm_panel()`](https://peteowen1.github.io/panna/reference/predict_spm_panel.md),
[`predict_spm_panel_net()`](https://peteowen1.github.io/panna/reference/predict_spm_panel_net.md),
[`predict_spm_panel_xgb()`](https://peteowen1.github.io/panna/reference/predict_spm_panel_xgb.md)
