# HTTP GET with exponential backoff retry

Wraps httr::GET with automatic retry on transient failures (5xx errors,
connection timeouts). Does NOT retry on rate limiting (429) or blocking
(403) as those require different handling.

## Usage

``` r
fetch_with_retry(url, max_retries = 3, base_delay = 1, max_delay = 30, ...)
```

## Arguments

- url:

  URL to fetch

- max_retries:

  Maximum number of retry attempts (default 3)

- base_delay:

  Initial delay in seconds before first retry (default 1)

- max_delay:

  Maximum delay between retries in seconds (default 30)

- ...:

  Additional arguments passed to httr::GET (headers, timeout, handle)

## Value

httr response object, or NULL with attributes on permanent failure
