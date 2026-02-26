# Upload parquet files to GitHub Releases

Uploads only parquet files (not RDS) to a GitHub Release. This is the
preferred upload method for efficient storage.

## Usage

``` r
pb_upload_parquet(
  repo = "peteowen1/pannadata",
  tag = "latest",
  source = NULL,
  verbose = TRUE
)
```

## Arguments

- repo:

  GitHub repository in "owner/repo" format

- tag:

  Release tag (default: "latest")

- source:

  Source directory containing data folder (default: pannadata_dir())

- verbose:

  Print progress messages

## Value

Invisible data frame with uploaded file info

## Examples

``` r
if (FALSE) { # \dontrun{
pb_upload_parquet()
} # }
```
