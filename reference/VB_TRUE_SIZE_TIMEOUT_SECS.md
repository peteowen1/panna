# Timeout for the authoritative asset-size read

[`.vb_asset_true_size()`](https://peteowen1.github.io/panna/reference/dot-vb_asset_true_size.md)
transfers one byte, so this bounds connection and redirect time, not
download time. Kept short deliberately: it runs only after the
post-upload verify has already exhausted its retries, and a slow answer
there must not extend a release job that is already late.

## Usage

``` r
VB_TRUE_SIZE_TIMEOUT_SECS
```

## Format

An object of class `numeric` of length 1.
