# Load match predictions

Loads match predictions from local cache or downloads from GitHub
Releases.

## Usage

``` r
load_predictions(source = c("remote", "local"), filter_future = FALSE)
```

## Arguments

- source:

  Data source: "local" loads from pannadata/predictions/ (errors if not
  found), "remote" always downloads latest from GitHub release first.

- filter_future:

  If TRUE, returns only matches with match_date \>= today.

## Value

Data frame of match predictions.

## Examples

``` r
if (FALSE) { # \dontrun{
# Load from remote (always downloads latest)
preds <- load_predictions(source = "remote")

# Load only future matches
preds <- load_predictions(source = "remote", filter_future = TRUE)

# Load from local cache
preds <- load_predictions(source = "local")
} # }
```
