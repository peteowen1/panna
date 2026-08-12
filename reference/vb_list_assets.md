# List release assets (uncached, typed errors)

The positive-confirmation primitive. 404 on the TAG raises
`vb_error_absent`; anything else raises `vb_error_transient`.

## Usage

``` r
vb_list_assets(repo, tag)
```

## Value

data.frame(name, size, updated_at, id)
