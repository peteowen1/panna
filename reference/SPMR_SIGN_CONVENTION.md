# Sign convention stamped into `career_spm.parquet` (SPMR)

Same idiom as `CAREER_PANNA_SIGN_CONVENTION`. Stamped so SPMR cannot
repeat what `career_rapm.parquet` did: that file was written 2026-09-03,
before the positive=good flip landed 2026-09-11, carries **no**
`sign_convention` column, and still reads inverted — it correlates
−0.989 with `panna_defense` and ranks Gabriel Magalhães 383rd of 383.
Nothing catches it because there is no tag to check. `09d_spmr.R` stamps
this at write time and `.assert_spmr_sign_convention()` aborts on a
stale or unmarked file.

## Usage

``` r
SPMR_SIGN_CONVENTION
```

## Format

An object of class `character` of length 1.
