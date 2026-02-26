# Get EPV Model Feature Columns

Returns the list of feature columns used for EPV model training. Uses
shared constants to ensure consistency with create_epv_features().

## Usage

``` r
get_epv_feature_cols(include_sequence = TRUE, n_prev = 3)
```

## Arguments

- include_sequence:

  Whether to include sequence features (default TRUE)

- n_prev:

  Number of previous actions for sequence features (default 3)

## Value

Character vector of feature column names
