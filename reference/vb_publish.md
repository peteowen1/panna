# Manifest-last atomic publish (the versebus producer pattern)

Ordered: hash -\> floor-check -\> upload data assets (bounded retries,
collect failures) -\> gate (any failure aborts BEFORE the manifest, so
consumers keep the last consistent snapshot) -\> verify live asset list
-\> upload bus_manifest.json LAST -\> fire the cache-invalidation hook
(`options(versebus.on_publish = function(repo, tag) ...)`).

## Usage

``` r
vb_publish(
  paths,
  repo,
  tag,
  rows = NULL,
  carry_forward = TRUE,
  min_row_frac = NULL,
  max_retries = 2,
  dry_run = FALSE
)
```

## Arguments

- paths:

  character vector of local files to upload

- rows:

  optional named integer vector: rows per basename

- carry_forward:

  merge with the previous manifest so partial publishes still describe
  the whole tag

- min_row_frac:

  optional floor vs previous manifest rows (e.g. 0.9)

- dry_run:

  build + return the manifest without uploading anything
