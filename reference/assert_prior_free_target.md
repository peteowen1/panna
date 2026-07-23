# Abort unless a target artifact is provenance-stamped prior-free RAPM

Static circularity guard (BOX-SCORE-VALUE-SPM-REDESIGN.md sec 2.4.1):
[`fit_rapm_with_prior()`](https://peteowen1.github.io/panna/reference/fit_rapm_with_prior.md)
(`R/rapm_model.R:389`) shrinks toward an SPM prior, so xRAPM and career
panna embed box-stat information – regressing box features onto them
(directly or via any downstream SPM panel target) would close the SPM
-\> prior -\> posterior -\> "prior-free" target loop. Any box-score
value training entry point (e.g. the planned
[`fit_spm_panel()`](https://peteowen1.github.io/panna/reference/fit_spm_panel.md))
must call this on its target argument before fitting.

## Usage

``` r
assert_prior_free_target(target)
```

## Arguments

- target:

  The candidate target artifact – a `04b` vintage list, the top-level
  `04b` list, or a raw
  [`fit_rapm()`](https://peteowen1.github.io/panna/reference/fit_rapm.md)/[`fit_rapm_with_prior()`](https://peteowen1.github.io/panna/reference/fit_rapm_with_prior.md)
  model object.

## Value

Invisibly `TRUE` if the target is accepted.

## Details

Two accepted shapes: (1) a `04b_rapm_window_targets.R` vintage element
(or the top-level list), stamped
`target_provenance = "prior_free_rapm_window"` by that script and
nowhere else; (2) a raw
[`fit_rapm()`](https://peteowen1.github.io/panna/reference/fit_rapm.md)
model object (legacy path) whose `panna_metadata$type == "rapm"` with no
`used_prior` field –
[`fit_rapm_with_prior()`](https://peteowen1.github.io/panna/reference/fit_rapm_with_prior.md)
always sets `used_prior = TRUE`,
[`fit_rapm()`](https://peteowen1.github.io/panna/reference/fit_rapm.md)
never sets it, so its absence is the discriminator. Anything else
(including `type == "xrapm"`/`"xrapm_net"`, or no provenance at all)
aborts.

## See also

Other rapm:
[`create_all_splints()`](https://peteowen1.github.io/panna/reference/create_all_splints.md),
[`create_rapm_design_matrix()`](https://peteowen1.github.io/panna/reference/create_rapm_design_matrix.md),
[`extract_period_end_times()`](https://peteowen1.github.io/panna/reference/extract_period_end_times.md),
[`extract_player_timing_from_events()`](https://peteowen1.github.io/panna/reference/extract_player_timing_from_events.md),
[`extract_rapm_ratings()`](https://peteowen1.github.io/panna/reference/extract_rapm_ratings.md),
[`fit_rapm()`](https://peteowen1.github.io/panna/reference/fit_rapm.md),
[`fit_rapm_with_prior()`](https://peteowen1.github.io/panna/reference/fit_rapm_with_prior.md)
