# Download parquet files from GitHub Releases

Downloads the parquet zip file from a GitHub Release and extracts it.

## Usage

``` r
pb_download_parquet(
  repo = "peteowen1/pannadata",
  tag = "latest",
  dest = NULL,
  verbose = TRUE
)
```

## Arguments

- repo:

  GitHub repository in "owner/repo" format

- tag:

  Release tag (default: "latest")

- dest:

  Destination directory (default: pannadata_dir())

- verbose:

  Print progress messages

## Value

Invisible path to destination directory

## Examples

``` r
if (FALSE) { # \dontrun{
pb_download_parquet()
pb_download_parquet(dest = "~/football-data")
} # }
```
