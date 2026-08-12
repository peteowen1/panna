# Cache validity = sidecar sha matches the manifest entry

Trusts the `.sha256` sidecar rather than rehashing `local_path` on every
call (rehashing a multi-GB model on every load would defeat the point of
caching). As a cheap corroborating check, a `local_path` modified more
recently than its sidecar is treated as invalid – the sidecar can only
describe content at-or-before its own write time.

## Usage

``` r
vb_cache_validate(local_path, manifest_entry)
```
