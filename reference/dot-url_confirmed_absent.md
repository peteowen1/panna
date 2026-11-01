# Confirm a URL is genuinely absent

POSITIVE confirmation only. A HEAD request that definitively reports the
resource gone (404/410) is the ONLY thing that returns `TRUE`; every
other outcome – 200, a 5xx, a timeout, a DNS failure – returns `FALSE`
so the caller goes on treating the failure as transient.

Getting this backwards is the expensive direction: a caller that reads
"network blip" as "the file does not exist" overwrites full-history
ratings-data with just the seasons one run computed (torp P1/P8). So the
fail-safe answer is FALSE.

## Usage

``` r
.url_confirmed_absent(url)
```

## Arguments

- url:

  A single URL.

## Value

`TRUE` only when the server positively reports the resource absent.
