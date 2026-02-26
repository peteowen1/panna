# Upload parquet files by source type

Uploads parquet files to source-specific GitHub releases.

- "fbref": FBref data to fbref-latest tag

- "understat": Understat data to understat-latest tag

- "all": All data to latest tag (legacy behavior)

## Usage

``` r
pb_upload_source(
  source_type = c("fbref", "understat", "opta", "all"),
  repo = "peteowen1/pannadata",
  source = NULL,
  verbose = TRUE
)
```

## Arguments

- source_type:

  Data source: "fbref", "understat", or "all"

- repo:

  GitHub repository in "owner/repo" format

- source:

  Source directory containing data folder (default: pannadata_dir())

- verbose:

  Print progress messages

## Value

Invisible data frame with uploaded file info

## Examples

``` r
if (FALSE) { # \dontrun{
# Upload FBref data only
pb_upload_source("fbref")

# Upload Understat data only
pb_upload_source("understat")

# Upload all data (legacy)
pb_upload_source("all")
} # }
```
