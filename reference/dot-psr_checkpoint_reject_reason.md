# Why a checkpoint was rejected (empty string = usable)

Split from
[`.psr_checkpoint_usable()`](https://peteowen1.github.io/panna/reference/dot-psr_checkpoint_usable.md)
so a resume failure logs the ACTUAL reason. The message used to say
"fingerprint mismatch" unconditionally, which is misleading when the
real cause was a truncated checkpoint or an out-of-range position –
exactly the situation where someone reading the log is trying to work
out why a long run restarted from scratch.

## Usage

``` r
.psr_checkpoint_reject_reason(cp, fingerprint, n_dates)
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

Empty string if usable, else a short human-readable reason.
