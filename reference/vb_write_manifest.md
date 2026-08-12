# Write a bus_manifest.json to `path`

Write a bus_manifest.json to `path`

## Usage

``` r
vb_write_manifest(
  entries,
  tag,
  path,
  producer = vb_producer_info(),
  notes = ""
)
```

## Arguments

- entries:

  list of vb_asset_entry() results (the FULL tag contents)
