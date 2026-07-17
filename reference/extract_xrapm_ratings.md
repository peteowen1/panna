# Extract xRAPM ratings (with prior)

Extracts player ratings from a model fit with SPM prior. The final
coefficient is gamma + prior, where gamma is the deviation.

## Usage

``` r
extract_xrapm_ratings(model, lambda = "min")
```

## Arguments

- model:

  Fitted xRAPM model from fit_rapm_with_prior

- lambda:

  Which lambda to use ("min" or "1se")

## Value

Data frame with player ratings including deviation from prior. In
`mode = "net"`, `offense`/`defense`/deviation/prior columns are `NA` and
`xrapm` holds the net coefficient.

## Details

F4 (FABLE-PRIOR-FIX-PLAN.md review): mode-aware via the model's stored
metadata (`model$panna_metadata$mode`, or `type == "xrapm_net"` for
older/hand-built metadata without a `mode` field). In `mode = "od"`
(default), `xrapm = offense - defense` as before. In `mode = "net"`,
there is no offense/defense split (D2) – the single `_net` coefficient
per player IS the rating, and
`offense`/`defense`/`off_deviation`/`def_deviation`/
`off_prior`/`def_prior` are set `NA` (they have no meaning against a
design with no offense/defense split). Aborts if the model's coefficient
names don't actually match the declared mode – indexing only
`_off`/`_def` names on a net-mode fit previously returned silently
all-NA ratings with no error (the bug this fixes).

## See also

Other panna ratings:
[`add_value_metrics_to_splints()`](https://peteowen1.github.io/panna/reference/add_value_metrics_to_splints.md),
[`aggregate_season_ratings()`](https://peteowen1.github.io/panna/reference/aggregate_season_ratings.md)
