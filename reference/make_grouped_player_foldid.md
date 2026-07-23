# Assign player-grouped CV fold ids: every row for a given `player_id` lands in the same fold

BOX-SCORE-VALUE-SPM-REDESIGN.md sec 2.2/R5: overlapping vintage windows
make rows for the same player heavily dependent (a player's 2023 and
2024 panel rows share 4 of 5 window seasons); random row-level CV folds
leak across folds and inflate CV-selected lambda's apparent fit. Folds
are assigned by shuffling UNIQUE player ids into `nfolds` buckets
([`rep_len()`](https://rdrr.io/r/base/rep.html) +
[`sample()`](https://rdrr.io/r/base/sample.html), balanced), then
broadcasting per row.

## Usage

``` r
make_grouped_player_foldid(player_id, nfolds = 5, seed = NULL)
```

## Arguments

- player_id:

  Character vector, one entry per panel row. Coerced via
  [`as.character()`](https://rdrr.io/r/base/character.html); aborts on
  any `NA` (an `NA` player_id can't be consistently grouped – every
  occurrence would collide into the same "fold", silently defeating the
  R5 guarantee this function exists for).

- nfolds:

  Number of folds.

- seed:

  Optional RNG seed.

## Value

Integer vector (1..nfolds), same length as `player_id`.

## See also

Other spm panel:
[`assert_asof_panel_window()`](https://peteowen1.github.io/panna/reference/assert_asof_panel_window.md),
[`assert_grouped_player_folds()`](https://peteowen1.github.io/panna/reference/assert_grouped_player_folds.md),
[`build_spm_panel()`](https://peteowen1.github.io/panna/reference/build_spm_panel.md),
[`classify_role_group()`](https://peteowen1.github.io/panna/reference/classify_role_group.md),
[`fit_spm_panel()`](https://peteowen1.github.io/panna/reference/fit_spm_panel.md),
[`fit_spm_panel_xgb()`](https://peteowen1.github.io/panna/reference/fit_spm_panel_xgb.md),
[`predict_spm_panel()`](https://peteowen1.github.io/panna/reference/predict_spm_panel.md),
[`predict_spm_panel_net()`](https://peteowen1.github.io/panna/reference/predict_spm_panel_net.md),
[`predict_spm_panel_xgb()`](https://peteowen1.github.io/panna/reference/predict_spm_panel_xgb.md)
