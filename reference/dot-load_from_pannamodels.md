# Load a model from pannamodels without hiding why it failed

The model loaders try pannamodels first and fall back to a local copy.
They used to swallow every pannamodels error, including a failed sha256
integrity check, and quietly use the local file, which is often an older
vintage (review R052/R091). An integrity failure now aborts; any other
error is reported before the caller falls back.

## Usage

``` r
.load_from_pannamodels(
  name,
  loader = function(n) pannamodels::load_panna_model(n, verbose = FALSE)
)
```

## Arguments

- name:

  Model name passed to
  [`pannamodels::load_panna_model()`](https://rdrr.io/pkg/pannamodels/man/load_panna_model.html).

- loader:

  Function taking `name` and returning the model; defaults to
  pannamodels. Tests inject a stub so they run without pannamodels.

## Value

The model, or `NULL` when pannamodels is not installed or the load
failed for a reason other than integrity.
