# Download parquet files by source type

Downloads parquet files from source-specific GitHub releases.

## Usage

``` r
pb_download_source(
  source_type = c("fbref", "understat", "opta", "all"),
  repo = "peteowen1/pannadata",
  dest = NULL,
  verbose = TRUE
)
```

## Arguments

- source_type:

  Data source: "fbref", "understat", or "all"

- repo:

  GitHub repository in "owner/repo" format

- dest:

  Destination directory (default: pannadata_dir())

- verbose:

  Print progress messages

## Value

Invisible path to destination directory

## Examples

``` r
if (FALSE) { # \dontrun{
# Download FBref data only
pb_download_source("fbref")

# Download Understat data only
pb_download_source("understat")
} # }
```
