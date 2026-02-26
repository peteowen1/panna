# Get browser headers for FBref requests

Returns headers that mimic a real browser to avoid Cloudflare blocking.
Rotates through different User-Agent strings for safety.

## Usage

``` r
get_fbref_headers(referer = "https://fbref.com/en/comps/")
```

## Arguments

- referer:

  Optional referer URL (default: FBref competitions page)

## Value

Named character vector of HTTP headers
