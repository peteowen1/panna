# Row-count floor guard for read-modify-write upserts

Call immediately before uploading an accumulated table over an existing
one. Shrinkage beyond `floor` means the "existing" read was partial or
the merge dropped history – abort rather than wipe.

## Usage

``` r
vb_guard_accumulate(existing_df, combined_df, floor = 0.9)
```
