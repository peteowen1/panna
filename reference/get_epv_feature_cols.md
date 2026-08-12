# Get EPV Model Feature Columns

Returns the list of feature columns used for EPV model training. Uses
shared constants to ensure consistency with create_epv_features().

## Usage

``` r
get_epv_feature_cols(include_sequence = TRUE, n_prev = EPV_N_PREV)
```

## Arguments

- include_sequence:

  Whether to include sequence features (default TRUE)

- n_prev:

  Number of previous actions for sequence features (default
  `EPV_N_PREV`; must match whatever
  [`create_epv_features()`](https://peteowen1.github.io/panna/reference/create_epv_features.md)
  was given, or the model asks for columns the builder never produced)

## Value

Character vector of feature column names
