# Load WP model

Resolution order, matching load_epv_model() / load_xpass_model():

1.  Explicit `path` (if supplied and the file exists)

2.  pannamodels package (preferred — distributes wp_model via the `epv`
    release tag, downloaded + cached on first call)

3.  Local fallback at `pannadata/data/opta/models/wp_model.rds`

## Usage

``` r
load_wp_model(path = NULL)
```

## Arguments

- path:

  Directory to load from. If NULL, tries pannamodels first then falls
  back to `pannadata/data/opta/models/`.

## Value

WP model list (model + feature_names).
