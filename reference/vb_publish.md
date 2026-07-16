# Manifest-last atomic publish (the versebus producer pattern)

Ordered: hash → floor-check → upload data assets (bounded retries,
collect failures) → gate (any failure aborts BEFORE the manifest, so
consumers keep the last consistent snapshot) → verify live asset list →
upload bus_manifest.json LAST → fire the cache-invalidation hook
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

## See also

Other versebus:
[`vb_asset_entry()`](https://peteowen1.github.io/panna/reference/vb_asset_entry.md),
[`vb_atomic_write()`](https://peteowen1.github.io/panna/reference/vb_atomic_write.md),
[`vb_cache_validate()`](https://peteowen1.github.io/panna/reference/vb_cache_validate.md),
[`vb_classify_error()`](https://peteowen1.github.io/panna/reference/vb_classify_error.md),
[`vb_confirm_absent()`](https://peteowen1.github.io/panna/reference/vb_confirm_absent.md),
[`vb_download()`](https://peteowen1.github.io/panna/reference/vb_download.md),
[`vb_generation()`](https://peteowen1.github.io/panna/reference/vb_generation.md),
[`vb_guard_accumulate()`](https://peteowen1.github.io/panna/reference/vb_guard_accumulate.md),
[`vb_list_assets()`](https://peteowen1.github.io/panna/reference/vb_list_assets.md),
[`vb_producer_info()`](https://peteowen1.github.io/panna/reference/vb_producer_info.md),
[`vb_read_manifest()`](https://peteowen1.github.io/panna/reference/vb_read_manifest.md),
[`vb_read_prev_manifest()`](https://peteowen1.github.io/panna/reference/vb_read_prev_manifest.md),
[`vb_sha256()`](https://peteowen1.github.io/panna/reference/vb_sha256.md),
[`vb_write_manifest()`](https://peteowen1.github.io/panna/reference/vb_write_manifest.md)
