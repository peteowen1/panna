# Read a tag's bus_manifest.json (NULL + one-time warning when absent)

Applies the momentary-absence rule: if this session has previously seen
a manifest on the tag and it now looks absent (piggyback
delete-then-upload window), retry once after 10 s before declaring
legacy mode. A tag never having a manifest is always legacy mode, never
an error – `required` is accepted for caller compatibility but does not
abort on absence; a caller that needs to refuse an *uncommitted* asset
checks the returned manifest itself (see
[`vb_download()`](https://peteowen1.github.io/panna/reference/vb_download.md)'s
own require_manifest handling).

## Usage

``` r
vb_read_manifest(repo, tag, required = FALSE)
```
