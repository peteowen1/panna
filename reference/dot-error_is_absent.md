# Did this download error mean "absent"?

Classifies a failed parquet read as absent-vs-transient.

WHY THIS IS NOT JUST A REGEX. The message check is a fast path, not the
authority. R's
[`download.file()`](https://rdrr.io/r/utils/download.file.html) route
reports the HTTP status in a *warning* ("HTTP status was '404 Not
Found'") and leaves the error itself as a bare "cannot open URL '...'",
so a message regex sees no 404 and silently classifies a genuinely
missing file as transient. That misrouted every caller relying on
`vb_error_absent`: building a brand-new rating vintage aborted six times
over, because the not-yet-written torp_ratings\_.parquet read as a
network failure rather than as the legitimately-absent file it was
(2026-08-18, staging EPV v3).

Widening the regex would just move the brittleness, so absence is
confirmed against the resource itself.

## Usage

``` r
.error_is_absent(e, url)
```

## Arguments

- e:

  The condition raised by the failed read.

- url:

  The URL that was being read.

## Value

`TRUE` if the resource is confirmed absent.
