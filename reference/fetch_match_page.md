# Fetch FBref match page HTML

Makes HTTP request with browser headers to avoid Cloudflare blocking.

## Usage

``` r
fetch_match_page(match_url, timeout = 30)
```

## Arguments

- match_url:

  FBref match URL

- timeout:

  Request timeout in seconds (default 30)

## Value

Parsed HTML document (rvest xml_document) or NULL on failure
