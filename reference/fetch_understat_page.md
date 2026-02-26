# Fetch Understat page HTML

Makes HTTP request with browser headers.

## Usage

``` r
fetch_understat_page(url, timeout = 30)
```

## Arguments

- url:

  Understat URL

- timeout:

  Request timeout in seconds (default 30)

## Value

Parsed HTML document (rvest xml_document) or NULL on failure
