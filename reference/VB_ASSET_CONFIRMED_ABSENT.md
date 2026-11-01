# Sentinel: the download path positively reported the asset absent

[`.vb_asset_true_size()`](https://peteowen1.github.io/panna/reference/dot-vb_asset_true_size.md)
answers a byte count, `NA_real_` for "could not tell", and this for "the
server said 404/410". Those last two must not collapse together: callers
read "could not tell" as a reason to warn and proceed, so a genuinely
lost upload answering 404 would be reported as a successful write.
Negative so it cannot be mistaken for a real byte count.

## Usage

``` r
VB_ASSET_CONFIRMED_ABSENT
```

## Format

An object of class `numeric` of length 1.

## Details

It is NOT collision-proof against every comparison: being negative, it
satisfies `true_bytes < local_bytes` and so reads as "a truncated
upload" unless callers exclude it first. Every size comparison against
this value must test for the sentinel explicitly.
