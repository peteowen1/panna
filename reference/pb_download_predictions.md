# Download match predictions from GitHub Releases

Downloads predictions.parquet from the predictions-latest release on
peteowen1/pannadata.

## Usage

``` r
pb_download_predictions(
  repo = "peteowen1/pannadata",
  tag = "predictions-latest",
  dest = NULL
)
```

## Arguments

- repo:

  GitHub repository in "owner/repo" format.

- tag:

  Release tag (default: "predictions-latest").

- dest:

  Destination directory. If NULL, uses pannadata_dir()/predictions.

## Value

Invisibly returns the path to the downloaded file.

## Examples

``` r
if (FALSE) { # \dontrun{
pb_download_predictions()
} # }
```
