# Positively confirm an asset is absent from a release

TRUE only when the asset list was fetched successfully AND the name is
not in it (or the tag itself is confirmed 404). A listing failure raises
`vb_error_transient` – callers MUST NOT catch that into a default. This
is THE mandatory guard before any "start fresh / overwrite full-history"
branch.

## Usage

``` r
vb_confirm_absent(repo, tag, name)
```
