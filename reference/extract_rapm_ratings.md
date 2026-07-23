# Extract RAPM ratings from fitted model

Calculates player ratings as offense_coef - defense_coef (`mode = "od"`,
default). Positive = good, negative = bad.

## Usage

``` r
extract_rapm_ratings(model, lambda = "min")
```

## Arguments

- model:

  Fitted RAPM model from fit_rapm

- lambda:

  Which lambda to use ("min" or "1se")

## Value

Data frame with player ratings. In `mode = "net"`, `offense`/`defense`
are `NA` and `rapm` holds the net coefficient.

## Details

Mode-aware (FABLE-PRIOR-FIX-PLAN.md Step 5, mirroring
[`extract_xrapm_ratings()`](https://peteowen1.github.io/panna/reference/extract_xrapm_ratings.md)'s
F4 fix): a model fit against a `mode = "net"` design
([`create_rapm_design_matrix`](https://peteowen1.github.io/panna/reference/create_rapm_design_matrix.md)/[`prepare_rapm_data`](https://peteowen1.github.io/panna/reference/prepare_rapm_data.md))
has only `_net` coefficients – there is no offense/defense split to
extract (D2: the target is zero-sum by construction, so an od-style
split is mechanically unidentified; confirmed empirically: fitting a
true zero-sum target in `mode = "od"` drives `cor(offense, defense)` to
exactly -1). In `mode = "net"`, `rapm` holds the single net coefficient
and `offense`/`defense` are `NA`. Detected via
`model$panna_metadata$mode` (defaults `"od"` for models fit before this
field existed).

## See also

Other rapm:
[`assert_prior_free_target()`](https://peteowen1.github.io/panna/reference/assert_prior_free_target.md),
[`create_all_splints()`](https://peteowen1.github.io/panna/reference/create_all_splints.md),
[`create_rapm_design_matrix()`](https://peteowen1.github.io/panna/reference/create_rapm_design_matrix.md),
[`extract_period_end_times()`](https://peteowen1.github.io/panna/reference/extract_period_end_times.md),
[`extract_player_timing_from_events()`](https://peteowen1.github.io/panna/reference/extract_player_timing_from_events.md),
[`fit_rapm()`](https://peteowen1.github.io/panna/reference/fit_rapm.md),
[`fit_rapm_with_prior()`](https://peteowen1.github.io/panna/reference/fit_rapm_with_prior.md)
