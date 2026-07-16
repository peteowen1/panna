# List releases by source type

Shows available releases for different data sources.

## Usage

``` r
pb_list_sources(repo = "peteowen1/pannadata")
```

## Arguments

- repo:

  GitHub repository in "owner/repo" format

## Value

Data frame with release information by source

## See also

Other data distribution:
[`load_predictions()`](https://peteowen1.github.io/panna/reference/load_predictions.md),
[`pb_download_opta()`](https://peteowen1.github.io/panna/reference/pb_download_opta.md),
[`pb_download_predictions()`](https://peteowen1.github.io/panna/reference/pb_download_predictions.md)

## Examples

``` r
if (FALSE) { # \dontrun{
pb_list_sources()
} # }
```
