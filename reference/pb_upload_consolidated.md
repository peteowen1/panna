# Upload Consolidated Parquet Files to GitHub Releases

Uploads individual consolidated parquet files to a GitHub release. This
enables fast remote queries via
[`query_remote_parquet()`](https://peteowen1.github.io/panna/reference/query_remote_parquet.md).

## Usage

``` r
pb_upload_consolidated(
  source_dir = NULL,
  repo = "peteowen1/pannadata",
  tag = "fbref-latest",
  verbose = TRUE
)
```

## Arguments

- source_dir:

  Directory containing consolidated parquet files. Defaults to
  pannadata_dir()/consolidated.

- repo:

  GitHub repository in "owner/repo" format.

- tag:

  Release tag to upload to.

- verbose:

  Print progress messages.

## Value

Invisible data frame with upload results.

## Examples

``` r
if (FALSE) { # \dontrun{
# Build and upload consolidated parquets
build_consolidated_parquet()
pb_upload_consolidated()
} # }
```
