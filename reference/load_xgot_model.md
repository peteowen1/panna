# Load Pre-trained xGOT Model

Load Pre-trained xGOT Model

## Usage

``` r
load_xgot_model(path = NULL)
```

## Arguments

- path:

  Optional path to a model RDS. If NULL, tries pannamodels then the
  local pannadata models dir (mirrors load_xg_model()).

## Value

Fitted xGOT model, or NULL if unavailable.
