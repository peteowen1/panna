# How many WC 2026 matches are still unplayed?

Liveness test for the World Cup pipeline branch (steps 11/12/12b/12c in
`run_predictions_opta.R`). Those steps simulate a tournament in
progress; once the final is played there is nothing to simulate and
[`build_knockout_lookup()`](https://peteowen1.github.io/panna/reference/build_knockout_lookup.md)
aborts, because its constant-aggregates invariant only holds while every
WC row is an unplayed fixture carrying a single as-of snapshot. Played
rows carry per-match aggregates.

## Usage

``` r
.wc2026_fixtures_remaining(cache_dir)
```

## Arguments

- cache_dir:

  Predictions cache directory (holds `07_predictions.rds`).

## Value

Integer count of WC 2026 rows still marked `"fixture"` and carrying both
team names, or `NA_integer_` when the cache file is absent or lacks the
columns to answer. `NA` means "cannot tell" and callers should leave
their configuration alone rather than treating it as zero.

## Details

Reads step 07's prediction cache rather than the step 04 match dataset:
same answer, ~53k x 15 instead of ~47MB, in a pipeline with a history of
memory cliffs (panna#128).
