# Atomic local write: tempfile in dest's own dir, then rename

Same-directory rename is atomic on every filesystem we run on; writing
to tempdir() and renaming across devices is not. On any write failure
the destination is left untouched.

## Usage

``` r
vb_atomic_write(write_fn, dest)
```
