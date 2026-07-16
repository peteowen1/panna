# Positively confirm an asset is absent from a release

TRUE only when the asset list was fetched successfully AND the name is
not in it (or the tag itself is confirmed 404). A listing failure raises
`vb_error_transient` — callers MUST NOT catch that into a default. This
is THE mandatory guard before any "start fresh / overwrite full-history"
branch.

## Usage

``` r
vb_confirm_absent(repo, tag, name)
```

## See also

Other versebus:
[`vb_asset_entry()`](https://peteowen1.github.io/panna/reference/vb_asset_entry.md),
[`vb_atomic_write()`](https://peteowen1.github.io/panna/reference/vb_atomic_write.md),
[`vb_cache_validate()`](https://peteowen1.github.io/panna/reference/vb_cache_validate.md),
[`vb_classify_error()`](https://peteowen1.github.io/panna/reference/vb_classify_error.md),
[`vb_download()`](https://peteowen1.github.io/panna/reference/vb_download.md),
[`vb_generation()`](https://peteowen1.github.io/panna/reference/vb_generation.md),
[`vb_guard_accumulate()`](https://peteowen1.github.io/panna/reference/vb_guard_accumulate.md),
[`vb_list_assets()`](https://peteowen1.github.io/panna/reference/vb_list_assets.md),
[`vb_producer_info()`](https://peteowen1.github.io/panna/reference/vb_producer_info.md),
[`vb_publish()`](https://peteowen1.github.io/panna/reference/vb_publish.md),
[`vb_read_manifest()`](https://peteowen1.github.io/panna/reference/vb_read_manifest.md),
[`vb_read_prev_manifest()`](https://peteowen1.github.io/panna/reference/vb_read_prev_manifest.md),
[`vb_sha256()`](https://peteowen1.github.io/panna/reference/vb_sha256.md),
[`vb_write_manifest()`](https://peteowen1.github.io/panna/reference/vb_write_manifest.md)
