# Classify an error from a release/network operation

The cardinal versebus rule: when classification is uncertain, the answer
is "transient" (abort), never "absent" (overwrite). Only a positively
confirmed 404 on the tag/asset listing may classify as "absent".

## Usage

``` r
vb_classify_error(e)
```

## Arguments

- e:

  a condition object

## Value

one of "absent", "transient"

## See also

Other versebus:
[`vb_asset_entry()`](https://peteowen1.github.io/panna/reference/vb_asset_entry.md),
[`vb_atomic_write()`](https://peteowen1.github.io/panna/reference/vb_atomic_write.md),
[`vb_cache_validate()`](https://peteowen1.github.io/panna/reference/vb_cache_validate.md),
[`vb_confirm_absent()`](https://peteowen1.github.io/panna/reference/vb_confirm_absent.md),
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
