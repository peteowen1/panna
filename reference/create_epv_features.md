# Create Game State Features for EPV

Builds comprehensive features for each action including location,
sequence context, and match situation. Optimized with data.table.

## Usage

``` r
create_epv_features(spadl_actions, n_prev = EPV_N_PREV)
```

## Arguments

- spadl_actions:

  SPADL actions with chain assignments

- n_prev:

  Number of previous actions to include (default `EPV_N_PREV`). Must be
  a non-negative whole number.

## Value

Data frame with EPV features
