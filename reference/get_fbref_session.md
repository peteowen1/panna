# Get or create FBref session

Returns a persistent httr session handle that maintains cookies across
requests, like a real browser session.

## Usage

``` r
get_fbref_session(reset = FALSE)
```

## Arguments

- reset:

  If TRUE, creates a new session (default FALSE)

## Value

httr handle object
