# Decide whether a loaded checkpoint may be resumed from

Split out of
[`.estimate_prematch_skills_batch()`](https://peteowen1.github.io/panna/reference/dot-estimate_prematch_skills_batch.md)
so the decision that gates resuming can be tested directly – accepting a
checkpoint built from DIFFERENT inputs silently splices two computations
together (running sums from one set of inputs, later dates from another)
with no error and no visible symptom, so this is the single most
consequential branch in the streaming/resume path.

## Usage

``` r
.psr_checkpoint_usable(cp, fingerprint, n_dates)
```

## Arguments

- cp:

  The deserialized checkpoint (or `NULL` if it was missing or
  unreadable).

- fingerprint:

  The fingerprint computed for the CURRENT call.

- n_dates:

  Number of ref_dates in the current call, used to reject a checkpoint
  whose recorded position can't apply to this run.

## Value

`TRUE` only if the checkpoint is structurally complete and its
fingerprint matches exactly.
