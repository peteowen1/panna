# Retry a fallible operation with short exponential backoff

GitHub's release-asset CDN throws sporadic 5xx errors (torpdata#66/#68)
that usually clear on a second or third attempt with no code change –
this mirrors the backoff style `save_to_release()` already uses for its
upload-side 404/422 retry. Retries up to `times` attempts total,
sleeping `delays[i]` seconds before each retry (recycled if `times - 1`
exceeds `length(delays)`). `should_retry` lets the caller exclude
confirmed-absent failures (e.g. a real 404) – those never resolve by
waiting.

## Usage

``` r
.vb_retry(fn, times = 3L, delays = c(2, 5), should_retry = function(e) TRUE)
```

## Arguments

- fn:

  zero-arg function to attempt

- times:

  maximum attempts (default 3: one try + 2 retries)

- delays:

  seconds to sleep before each retry (default 2, then 5)

- should_retry:

  function(error) -\> logical; FALSE re-raises immediately
