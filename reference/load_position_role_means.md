# Load the bundled within-position normalization artifact

Per-role skill means built by `07b_build_position_means.R`. Pass the
result as `position_means` to `compute_player_psv`/ `compute_player_psr`
to enable BPM-style within-position scoring.

## Usage

``` r
load_position_role_means()
```

## Value

data.table(role, stat_name, mean), or NULL if the artifact is absent.
