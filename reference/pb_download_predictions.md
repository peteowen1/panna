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

## See also

Other data distribution:
[`load_predictions()`](https://peteowen1.github.io/panna/reference/load_predictions.md),
[`pb_download_opta()`](https://peteowen1.github.io/panna/reference/pb_download_opta.md),
[`pb_list_sources()`](https://peteowen1.github.io/panna/reference/pb_list_sources.md)

## Examples

``` r
if (FALSE) { # \dontrun{
pb_download_predictions()
} # }
```
