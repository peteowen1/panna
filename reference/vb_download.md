# Verified, atomic release-asset download

Downloads to a tempfile in `dest`'s directory, verifies (parquet magic
bytes; sha256 vs manifest when available, size vs asset list otherwise),
then atomically renames into place and writes a `<dest>.sha256` sidecar.
ON ANY FAILURE the temp is deleted and a pre-existing `dest` is left
untouched — but it is NEVER silently served as a fallback: the typed
error propagates and the caller must opt in to "serve stale + warn"
explicitly.

## Usage

``` r
vb_download(
  repo,
  tag,
  name,
  dest,
  manifest = NULL,
  require_manifest = isTRUE(Sys.getenv("VERSEBUS_STRICT") == "1"),
  max_age = NULL
)
```

## Arguments

- manifest:

  pass a manifest to verify sha256; NULL fetches it (legacy mode when
  the tag has none)

- require_manifest:

  TRUE in CI/strict mode (VERSEBUS_STRICT=1)

- max_age:

  optional difftime; manifest older than this raises `vb_error_stale`

## See also

Other versebus:
[`vb_asset_entry()`](https://peteowen1.github.io/panna/reference/vb_asset_entry.md),
[`vb_atomic_write()`](https://peteowen1.github.io/panna/reference/vb_atomic_write.md),
[`vb_cache_validate()`](https://peteowen1.github.io/panna/reference/vb_cache_validate.md),
[`vb_classify_error()`](https://peteowen1.github.io/panna/reference/vb_classify_error.md),
[`vb_confirm_absent()`](https://peteowen1.github.io/panna/reference/vb_confirm_absent.md),
[`vb_generation()`](https://peteowen1.github.io/panna/reference/vb_generation.md),
[`vb_guard_accumulate()`](https://peteowen1.github.io/panna/reference/vb_guard_accumulate.md),
[`vb_list_assets()`](https://peteowen1.github.io/panna/reference/vb_list_assets.md),
[`vb_producer_info()`](https://peteowen1.github.io/panna/reference/vb_producer_info.md),
[`vb_publish()`](https://peteowen1.github.io/panna/reference/vb_publish.md),
[`vb_read_manifest()`](https://peteowen1.github.io/panna/reference/vb_read_manifest.md),
[`vb_read_prev_manifest()`](https://peteowen1.github.io/panna/reference/vb_read_prev_manifest.md),
[`vb_sha256()`](https://peteowen1.github.io/panna/reference/vb_sha256.md),
[`vb_write_manifest()`](https://peteowen1.github.io/panna/reference/vb_write_manifest.md)
